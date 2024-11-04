#### Preamble ####
# Purpose: Linear Models for Prediction
# Author: Prankit Bhardwaj, Veyasan Ragulan, Chris Yong Hong Sen
# Date: 4 November 2024
# Contact: prankit.bhardwaj@mail.utoronto.ca 
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)
library(arrow)

#### Read data ####
just_harris_high_quality <- read_csv("data/02-analysis_data/analysis_data_harris.csv")
just_trump_high_quality <- read_csv("data/02-analysis_data/analysis_data_trump.csv")

### Model data ####
# Model 1 for Kamala: pct as a function of end_date, transparency_score and numeric_date
model_date <- lm(pct ~ end_date + transparency_score + numeric_grade, data = just_harris_high_quality)

# Model 2 for Kamala: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = just_harris_high_quality)

# Augment data with model predictions
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Model 1 for Trump: pct as a function of end_date, transparency_score and numeric_date
model_date_trump <- lm(pct ~ end_date  + transparency_score + numeric_grade, data = just_trump_high_quality)

# Model 2 for Trump: pct as a function of end_date and pollster
model_date_pollster_trump <- lm(pct ~ end_date + pollster, data = just_trump_high_quality)

# Augment Trump data with model predictions
just_trump_high_quality <- just_trump_high_quality |>
  mutate(
    fitted_date = predict(model_date_trump),
    fitted_date_pollster = predict(model_date_pollster_trump)
  )


#### Save Models ####
# Save each model to the models directory
saveRDS(model_date, file = "models/model_date_harris.rds")
saveRDS(model_date_pollster, file = "models/model_date_pollster_harris.rds")
saveRDS(model_date_trump, file = "models/model_date_trump.rds")
saveRDS(model_date_pollster_trump, file = "models/model_date_pollster_trump.rds")

#### Write Data to Parquet ####
# Write augmented Harris data to Parquet
write_parquet(just_harris_high_quality, "data/02-analysis_data/just_harris_high_quality.parquet")

# Write augmented Trump data to Parquet
write_parquet(just_trump_high_quality, "data/02-analysis_data/just_trump_high_quality.parquet")

# Write combined data (Harris and Trump) to Parquet
write_parquet(combined_data_with_preds, "data/02-analysis_data/combined_data_with_preds.parquet")

# Write swing states data to Parquet
write_parquet(swing_states_data, "data/02-analysis_data/swing_states_data.parquet")

