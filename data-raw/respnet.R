library(tidyverse)

raw <- read_csv("Rates_of_Laboratory-Confirmed_RSV__COVID-19__and_Flu_Hospitalizations_from_the_RESP-NET_Surveillance_Systems_20250112.csv") # nolint

respnet <- raw |>
  filter(
    `Age group` == "Overall",
    Sex == "Overall",
    `Race/Ethnicity` == "Overall",
    Site == "Overall",
    Type == "Unadjusted Rate"
  ) |>
  select(
    network = `Surveillance Network`,
    week_ending = `Week Ending Date`,
    rate = `Weekly Rate`
  ) |>
  mutate(across(week_ending, as.Date))

usethis::use_data(respnet, overwrite = TRUE)
