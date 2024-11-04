#### Preamble ####
# Purpose: Cleans and processes election polling data for Kamala Harris and Donald Trump
# Author: [Your Name]
# Date: [Current Date]
# Contact: [Your Email]
# License: MIT
# Pre-requisites: Requires raw election polling data in the "data/01-raw_data/" directory.

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)

#### Clean Data Function ####
clean_candidate_data <- function(data, candidate_name, declaration_date) {
  data |>
    filter(
      candidate_name == candidate_name,
      numeric_grade >= 2.7 # TODO: Investigate the choice of threshold for high-quality polls
    ) |>
    mutate(
      state = if_else(is.na(state), "National", state), # TODO: Review the state NA replacement logic for national polls
      end_date = mdy(end_date)
    ) |>
    filter(end_date >= as.Date(declaration_date)) |>
    mutate(
      num_supporters = round((pct / 100) * sample_size, 0) # Convert percentage to count of supporters
    ) |>
    rename(
      pollster_rating = numeric_grade # Renaming numeric_grade to pollster_rating for clarity
    )
}

#### Clean Data ####
# Clean data for Harris and Trump
just_harris_high_quality <- clean_candidate_data(data, "Kamala Harris", "2024-07-21")
just_trump_high_quality <- clean_candidate_data(data, "Donald Trump", "2024-06-01") # Update date if needed

#### Save Data ####
write_csv(just_harris_high_quality, "data/02-analysis_data/analysis_data_harris.csv")
write_csv(just_trump_high_quality, "data/02-analysis_data/analysis_data_trump.csv")
