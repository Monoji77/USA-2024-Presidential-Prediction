#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)

#### Read data ####
just_harris_high_quality <- read_csv("data/02-analysis_data/analysis_data_harris.csv")
just_trump_high_quality <- read_csv("data/02-analysis_data/analysis_data_trump.csv")

### Model data ####
base_plot <- ggplot(just_harris_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date")

# Plots poll estimates and overall smoothing
base_plot +
  geom_point() +
  geom_smooth()

# Color by pollster
# This gets messy - need to add a filter - see line 21
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

# Facet by pollster
# Make the line 21 issue obvious
# Also - is there duplication???? Need to go back and check
base_plot +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

# Color by pollscore
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")

base_plot_trump <- ggplot(just_trump_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Trump percent", x = "Date")

base_plot_trump +
  geom_point() +
  geom_smooth()

base_plot_trump +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

base_plot_trump +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

base_plot_trump +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")

#### Starter models ####
# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date + transparency_score + numeric_grade, data = just_harris_high_quality)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = just_harris_high_quality)

# Augment data with model predictions
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Model 1 for Trump: pct as a function of end_date
model_date_trump <- lm(pct ~ end_date  + transparency_score + numeric_grade, data = just_trump_high_quality)

# Model 2 for Trump: pct as a function of end_date and pollster
model_date_pollster_trump <- lm(pct ~ end_date + pollster, data = just_trump_high_quality)

# Augment Trump data with model predictions
just_trump_high_quality <- just_trump_high_quality |>
  mutate(
    fitted_date = predict(model_date_trump),
    fitted_date_pollster = predict(model_date_pollster_trump)
  )

# Plot model predictions for Kamala
# Model 1
ggplot(just_harris_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(just_harris_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# Plot model predictions for Trump
# Model 1
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "red", linetype = "dotted") +
  theme_classic() +
  labs(y = "Trump percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "red", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Trump percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# Plot model predictions for Trump
# Combine the datasets for Trump and Harris, keeping their respective predictions
combined_data_with_preds <- bind_rows(
  just_harris_high_quality |> mutate(candidate = "Kamala Harris"),
  just_trump_high_quality |> mutate(candidate = "Donald Trump")
)

# Plotting the model predictions for both Trump and Harris in one chart
ggplot(combined_data_with_preds, aes(x = end_date, y = pct)) +
  # Actual data points with transparency
  geom_point(aes(color = candidate), alpha = 0.5) +
  
  # Predicted lines for each candidate
  geom_line(data = combined_data_with_preds |> filter(candidate == "Kamala Harris"),
            aes(y = fitted_date_pollster, color = candidate), linetype = "solid", size = 1) +
  geom_line(data = combined_data_with_preds |> filter(candidate == "Donald Trump"),
            aes(y = fitted_date_pollster, color = candidate), linetype = "solid", size = 1) +
  
  # Set colors manually for better distinction
  scale_color_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red")) +
  
  # Add labels and theme
  labs(
    x = "Polling End Date",
    y = "Polling Percentage",
    title = "Polling Predictions for Kamala Harris and Donald Trump",
    color = "Candidate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plotting the smoothed predictions for both Trump and Harris
ggplot(combined_data_with_preds, aes(x = end_date, y = pct)) +
  # Actual data points with transparency
  geom_point(aes(color = candidate), alpha = 0.3) +
  
  # Smoothed lines for each candidate
  geom_smooth(aes(color = candidate), method = "loess", span = 0.2, se = FALSE, size = 1.2) +
  
  # Set colors manually for better distinction
  scale_color_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red")) +
  
  # Add labels and theme
  labs(
    x = "Polling End Date",
    y = "Polling Percentage",
    title = "Smoothed Polling Predictions for Kamala Harris and Donald Trump",
    color = "Candidate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# All of the above would be in scripts - data cleaning scripts and modelling scripts. 
# This is an example of how you get a results table that you could put into your Quarto doc
modelsummary(models = list("Model 1" = model_date, "Model 2" = model_date_pollster))

# Define major swing states
swing_states <- c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin")

combined_data <- bind_rows(
  just_harris_high_quality |> mutate(candidate = "Kamala Harris"),
  just_trump_high_quality |> mutate(candidate = "Donald Trump")
)

# Filter data for Harris and Trump in the swing states
swing_states_data <- combined_data |>
  filter(state %in% swing_states)

# Create a base plot for swing states data
swing_states_plot <- ggplot(swing_states_data, aes(x = end_date, y = pct, color = candidate)) +
  theme_minimal() +
  labs(x = "Polling End Date", y = "Polling Percentage", title = "Polling Outcomes in Swing States") +
  theme(legend.position = "bottom")

# Facet the plot by state
swing_states_plot +
  geom_point(aes(shape = candidate), alpha = 0.7) +
  geom_smooth(aes(linetype = candidate), se = FALSE) +
  facet_wrap(vars(state)) +
  scale_color_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red"))



#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


