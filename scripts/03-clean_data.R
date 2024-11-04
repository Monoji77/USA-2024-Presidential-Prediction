#### Preamble ####
# Purpose: Linear Models for Prediction
# Author: Prankit Bhardwaj, Veyasan Ragulan, Chris Yong Hong Sen
# Date: 4 November 2024
# Contact: prankit.bhardwaj@mail.utoronto.ca 
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

data <- read_csv("data/01-raw_data/raw_elections_data.csv")

just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.5 #
  
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state), # Hacky fix for national polls
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # When Harris declared
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) # Need number not percent for some models
  )

# Filter data to Trump estimates based on high-quality polls after he declared
just_trump_high_quality <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.5
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # Update with Trump's declaration date if needed
  mutate(
    num_trump = round((pct / 100) * sample_size, 0)
  )

#### Save Data ####
write_csv(just_harris_high_quality, "data/02-analysis_data/analysis_data_harris.csv")
write_csv(just_trump_high_quality, "data/02-analysis_data/analysis_data_trump.csv")
