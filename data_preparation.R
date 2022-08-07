

# Setup data

## https://gisumd.github.io/COVID-19-API-Documentation/
setwd("C:/LocalData/hema/git-projects/attractor-manuscript")

library(tidyverse)

countries_to_run <- "Finland"

# Ask for an indicator that's not found:
path <- "https://covidmap.umd.edu/api/resources?indicator=all&type=smoothed&country=Finland&daterange=20201115-20201130"
request <- httr::GET(url = path)
response <- httr::content(request, as = "text", encoding = "UTF-8")

# The error message contains all available indicators
returned_error <- jsonlite::fromJSON(response, flatten = TRUE) %>% data.frame()

# Pull available indicators
all_indicators <- returned_error %>% 
  dplyr::pull(error) %>% 
  stringr::str_replace_all(string = .,
                           pattern = "\'",
                           replacement = "") %>% 
  stringr::str_replace_all(string = .,
                           pattern = "need parameter:|indicator:",
                           replacement = "") %>% 
  stringr::str_replace_all(string = .,
                           pattern = "[]\\[]",
                           replacement = "") %>% 
  # Split based on "or", preceded or followed by any number of spaces 
  strsplit("[ \t]+or[ \t]+|[ \t]+or")  %>% 
  purrr::map(.x = .,
             .f = ~gsub(pattern = " ", replacement = "", x = .x)) %>% 
  unlist()

for(looped_country in countries_to_run) {
  
country_in_question <- looped_country
region_in_question <- "" # Leave empty for aggregate data

last_date_to_fetch <- "20231130" # For everything

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

worldsurvey_df <- list()

for (i in 1:length(all_indicators)){
  
  # add url
  path <- paste0("https://covidmap.umd.edu/api/resources?indicator=",
                 all_indicators[i],
                 "&type=daily&country=", country_in_question,
                 "&region=", region_in_question,
                 "&daterange=20200101-", last_date_to_fetch)
  
  # request data from api
  request <- httr::GET(url = path)
  
  # make sure the content is encoded with 'UTF-8'
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  
  # now we have a dataframe for use!
  worldsurvey_df[[i]] <- jsonlite::fromJSON(response, flatten = FALSE)[[1]] %>% 
    # If there's no data for the indicator, return empty data frame
    {if(is.list(.)) data.frame(.) else 
      data.frame(empty = character(),
                 data.survey_date = as.Date(character())) }
  
}

# Discard empty columns
worldsurvey_df_nonempty <- purrr::discard(worldsurvey_df, ~nrow(.) == 0)

worldsurvey_df_selected <- purrr::map(.x = worldsurvey_df_nonempty,
                                      .f = ~.x %>% 
                                        dplyr::select(1, "survey_date"))

worldsurvey_df_selected_full <- purrr::map(.x = worldsurvey_df_nonempty,
                                      .f = ~.x %>% 
                                        dplyr::select(1, std_error = 2, 
                                                      "survey_date") %>% 
                                        dplyr::mutate(name = names(.[[1]]))) %>%
  
  purrr::reduce(.x = .,
                .f = dplyr::full_join,
                by = "survey_date")

worldsurvey_analysis_allvars <- purrr::reduce(.x = worldsurvey_df_selected,
                                              .f = dplyr::full_join,
                                              by = "survey_date") %>% 
  dplyr::arrange(survey_date) %>% 
  dplyr::mutate(date = as.Date(survey_date, format = "%Y%m%d")) %>% 
  dplyr::select(-survey_date)

# # How many observations per variable?
# purrr::map(.x = worldsurvey_analysis_allvars,
#            .f = ~sum(!is.na(.x))) %>%
#   dplyr::bind_cols() %>%
#   tidyr::pivot_longer(cols = everything()) %>%
#   dplyr::arrange(value)

# # Show which indicator depicts which variable name
# purrr::map(.x = worldsurvey_df, .f = ~nrow(.) == 0) %>% 
#   dplyr::bind_cols() %>% 
#   tidyr::pivot_longer(cols = everything()) %>% 
#   dplyr::mutate(indicator = all_indicators) %>% 
#   dplyr::filter(value != TRUE) %>% 
#   dplyr::mutate(varname = names(worldsurvey_analysis_allvars)) %>% 
#   dplyr::select(indicator, varname)

# saveRDS(object = worldsurvey_analysis_allvars, 
#         file = "./data/worldsurvey_analysis_allvars.RDS")

data_for_pca <- worldsurvey_analysis_allvars %>%
  # Choose only the latest wave of the questionnaire
  dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(date,
                # Drop variables with more than ten missings
                where(~sum(is.na(.x)) <= 10),
                # Drop vaccination variables
                -contains("vaccin")) %>% 
  tidyr::drop_na()

data_for_pca_full <- worldsurvey_analysis_allvars %>%
  dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date)

# Data from all questionnaire waves, i.e. since 2020
data_for_pca_full_allwaves <- worldsurvey_analysis_allvars %>%
  # dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date)

latest_date <- data_for_pca$date %>% tail(1) 

readr::write_csv(x = data_for_pca,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS", ".csv"))

readr::write_csv(x = data_for_pca_full,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS", ".csv"))

# Data from all questionnaire waves, i.e. since 2020
readr::write_csv(x = data_for_pca_full_allwaves,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS_allwaves", ".csv"))

#### Coefficient of variation

worldsurvey_df_selected_cov <- purrr::map(
  .x = worldsurvey_df_nonempty,
  .f = ~.x %>%
    dplyr::select("survey_date",
                  1,
                  std_error = 2,
                  sample_size) %>%
    dplyr::mutate(std_dev = std_error * sqrt(sample_size),
                  coef_of_variation = std_dev/.[[2]])
)

worldsurvey_df_selected_cov_filtered <-
  purrr::map(.x = worldsurvey_df_selected_cov,
             .f = ~.x %>%
               dplyr::mutate(date = as.Date(survey_date,
                                            format = "%Y%m%d")) %>%
               dplyr::filter(date > "2021-06-08"))

coevar_data_from_2021_06_08_long <- 
  purrr::map(.x = worldsurvey_df_selected_cov_filtered,
             .f = ~.x %>%
               dplyr::mutate(name = names(.x[2]),
                             value = coef_of_variation) %>%
               dplyr::select(date, name, value,
                             sample_size, std_error)) %>%
  purrr::reduce(.x = .,
                .f = full_join) 

coevar_data_from_2021_06_08 <- coevar_data_from_2021_06_08_long %>%
  dplyr::select(-sample_size, -std_error) %>% 
  tidyr::pivot_wider() 

worldsurvey_sample_sizes_from_2021_06_08 <- 
  coevar_data_from_2021_06_08_long %>%
  dplyr::select(date,
                name,
                sample_size)

worldsurvey_std_errors_from_2021_06_08 <- 
  coevar_data_from_2021_06_08_long %>%
  dplyr::select(date,
                name,
                std_error)

coevar_data_from_2021_06_08_dropna <- coevar_data_from_2021_06_08 %>%
  dplyr::select(date,
                where(~sum(is.na(.x)) <= 10),
                -contains("vaccin")) %>% 
  tidyr::drop_na()

latest_date_coevar <- coevar_data_from_2021_06_08_dropna$date %>% max()

# coevar_data_from_2021_06_08_dropna %>%
#   readr::write_csv(file = paste0(
#     "./data/", 
#     country_in_question, region_in_question, 
#     "_worldsurvey_nonmissing_c_of_v_since_2021-06-08_to_",
#     latest_date_coevar, ".csv"))
# coevar_data_from_2021_06_08 %>%
#   readr::write_csv(file = paste0(
#     "./data/", 
#     country_in_question, region_in_question, 
#     "_worldsurvey_allvars_c_of_v_since_2021-06-08_to_",
#     latest_date_coevar, ".csv"))

latest_date_n <- worldsurvey_sample_sizes_from_2021_06_08$date %>% max()
worldsurvey_sample_sizes_from_2021_06_08 %>% 
  readr::write_csv(file = paste0(
    "./data/", 
    country_in_question, region_in_question, 
    "_sample_sizes_since_2021-06-08_to_",
    latest_date_n, ".csv"))

latest_date_se <- worldsurvey_std_errors_from_2021_06_08$date %>% max()
worldsurvey_std_errors_from_2021_06_08 %>% 
  readr::write_csv(file = paste0(
    "./data/", 
    country_in_question, region_in_question, 
    "_std_errors", ".csv"))

####### UNWEIGHTED DATA

unweighted_worldsurvey_df_selected <- purrr::map(.x = worldsurvey_df_nonempty,
                                          .f = ~.x %>% 
                                            dplyr::select(3, "survey_date"))

unweighted_worldsurvey_df_selected_full <- purrr::map(
  .x = worldsurvey_df_nonempty,
  .f = ~.x %>% 
    dplyr::select(3, std_error = 4, 
                  "survey_date") %>% 
    dplyr::mutate(name = names(.[[1]]))) %>%
  
  purrr::reduce(.x = .,
                .f = dplyr::full_join,
                by = "survey_date")

unweighted_worldsurvey_analysis_allvars <- purrr::reduce(
  .x = unweighted_worldsurvey_df_selected,
  .f = dplyr::full_join,
  by = "survey_date") %>% 
  dplyr::arrange(survey_date) %>% 
  dplyr::mutate(date = as.Date(survey_date, format = "%Y%m%d")) %>% 
  dplyr::select(-survey_date)

# # How many observations per variable?
# purrr::map(.x = worldsurvey_analysis_allvars,
#            .f = ~sum(!is.na(.x))) %>%
#   dplyr::bind_cols() %>%
#   tidyr::pivot_longer(cols = everything()) %>%
#   dplyr::arrange(value)

# # Show which indicator depicts which variable name
# purrr::map(.x = worldsurvey_df, .f = ~nrow(.) == 0) %>% 
#   dplyr::bind_cols() %>% 
#   tidyr::pivot_longer(cols = everything()) %>% 
#   dplyr::mutate(indicator = all_indicators) %>% 
#   dplyr::filter(value != TRUE) %>% 
#   dplyr::mutate(varname = names(worldsurvey_analysis_allvars)) %>% 
#   dplyr::select(indicator, varname)

### DATA FOR PCA
unweighted_data_for_pca <- unweighted_worldsurvey_analysis_allvars %>%
  dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(date,
                # Drop variables with less than 10 missing values
                where(~sum(is.na(.x)) <= 10),
                # Drop vaccination-related variables
                -contains("vaccin")) %>% 
  tidyr::drop_na()

unweighted_data_for_pca_full <- unweighted_worldsurvey_analysis_allvars %>%
  dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date)

# Data from all questionnaire waves, i.e. since 2020
unweighted_data_for_pca_full_allwaves <- unweighted_worldsurvey_analysis_allvars %>%
  # dplyr::filter(date > "2021-06-08") %>% 
  dplyr::arrange(date)

unweighted_latest_date <- unweighted_data_for_pca$date %>% tail(1) 

readr::write_csv(x = unweighted_data_for_pca,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS_unweighted",
                                ".csv"))

readr::write_csv(x = unweighted_data_for_pca_full,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS_unweighted",
                               ".csv"))

# Data from all questionnaire waves, i.e. since 2020
readr::write_csv(x = unweighted_data_for_pca_full_allwaves,
                 file = paste0("./data/",
                               country_in_question, region_in_question, 
                               "_CTIS_unweighted_allwaves",
                               ".csv"))
#### Coefficient of variation

unweighted_worldsurvey_df_selected_cov <- purrr::map(
  .x = worldsurvey_df_nonempty,
  .f = ~.x %>%
    dplyr::select("survey_date",
                  3,
                  std_error = 4,
                  sample_size) %>%
    dplyr::mutate(std_dev = std_error * sqrt(sample_size),
                  coef_of_variation = std_dev/.[[2]])
)

unweighted_worldsurvey_df_selected_cov_filtered <-
  purrr::map(.x = unweighted_worldsurvey_df_selected_cov,
             .f = ~.x %>%
               dplyr::mutate(date = as.Date(survey_date,
                                            format = "%Y%m%d")) %>%
               dplyr::filter(date > "2021-06-08"))

unweighted_coevar_data_from_2021_06_08_long <- 
  purrr::map(.x = unweighted_worldsurvey_df_selected_cov_filtered,
             .f = ~.x %>%
               dplyr::mutate(name = names(.x[2]),
                             value = coef_of_variation) %>%
               dplyr::select(date, name, value,
                             sample_size, std_error)) %>%
  purrr::reduce(.x = .,
                .f = full_join) 

unweighted_coevar_data_from_2021_06_08 <- 
  unweighted_coevar_data_from_2021_06_08_long %>%
  dplyr::select(-sample_size, -std_error) %>% 
  tidyr::pivot_wider() 

unweighted_worldsurvey_sample_sizes_from_2021_06_08 <- 
  unweighted_coevar_data_from_2021_06_08_long %>%
  dplyr::select(date,
                name,
                sample_size)

unweighted_worldsurvey_std_errors_from_2021_06_08 <- 
  unweighted_coevar_data_from_2021_06_08_long %>%
  dplyr::select(date,
                name,
                std_error)

unweighted_coevar_data_from_2021_06_08_dropna <- 
  unweighted_coevar_data_from_2021_06_08 %>%
  dplyr::select(date,
                where(~sum(is.na(.x)) <= 10),
                -contains("vaccin")) %>% 
  tidyr::drop_na()

latest_date_n <- unweighted_worldsurvey_sample_sizes_from_2021_06_08$date %>% max()
unweighted_worldsurvey_sample_sizes_from_2021_06_08 %>% 
  readr::write_csv(file = paste0(
    "./data/", 
    country_in_question, region_in_question, 
    "_sample_sizes_unweighted", ".csv"))

latest_date_se <- unweighted_worldsurvey_std_errors_from_2021_06_08$date %>% max()
unweighted_worldsurvey_std_errors_from_2021_06_08 %>% 
  readr::write_csv(file = paste0(
    "./data/", 
    country_in_question, region_in_question, 
    "_std_errors_unweighted", ".csv"))

}