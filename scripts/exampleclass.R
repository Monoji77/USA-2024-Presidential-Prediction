#### Set-up ####
# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)


#### Prepare dataset ####
# Read in the data and clean variable names
data <- read_csv("scripts/president_polls.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 1.5 # Need to investigate this choice - come back and fix. 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state), # Hacky fix for national polls - come back and check
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
    numeric_grade >= 1.5
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # Update with Trump's declaration date if needed
  mutate(
    num_trump = round((pct / 100) * sample_size, 0)
  )

#### Plot data ####
base_plot <- ggplot(just_harris_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date")

base_plot_trump <- ggplot(just_trump_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Trump percent", x = "Date")

# Plots poll estimates and overall smoothing
base_plot +
  geom_point() +
  geom_smooth()

base_plot_trump +
  geom_point() +
  geom_smooth()


# Color by pollster
# This gets messy - need to add a filter - see line 21
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

base_plot_trump +
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

base_plot_trump +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

# Color by pollscore
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")

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
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Trump percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Trump percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# Plot model predictions for Trump
# Combine the datasets for Trump and Harris, keeping their respective predictions
combined_data_with_preds <- bind_rows(
  just_harris_high_quality |> mutate(candidate = "Kamala Harris"),
  just_trump_high_quality |> mutate(candidate = "Donald Trump")
)


# Define major swing states
swing_states <- c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin")

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


#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

# Model 1
model_formula_1 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster)

# Model 2
model_formula_2 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = just_harris_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = just_harris_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)
# Note the warnings here and follow the instructions for dealing with it - come back and fix.
# Change 'pollster' and 'state' to factor variables for Trump data
just_trump_high_quality <- just_trump_high_quality |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

# Model 1 for Trump: binomial outcome with pollster as random effect
model_formula_1_trump <- cbind(num_trump, sample_size - num_trump) ~ (1 | pollster)

# Model 2 for Trump: add state as a random effect
model_formula_2_trump <- cbind(num_trump, sample_size - num_trump) ~ (1 | pollster) + (1 | state)

# Fit Bayesian models
bayesian_model_1_trump <- stan_glmer(
  formula = model_formula_1_trump,
  data = just_trump_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2_trump <- stan_glmer(
  formula = model_formula_2_trump,
  data = just_trump_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

# Posterior predictive checks
pp_check(bayesian_model_1)
pp_check(bayesian_model_2)
pp_check(bayesian_model_1_trump)
pp_check(bayesian_model_2_trump)

# Summarize the model
summary(bayesian_model_1)
summary(bayesian_model_2)
summary(bayesian_model_1_trump)
summary(bayesian_model_2_trump)

# Plot random effects
plot(bayesian_model_1, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_1_trump, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_2_trump, pars = "(Intercept)", prob = 0.95)

# Model summary works the same as above for Bayesian models.


#### Bayesian models and splines ####
# Change date to be number of days since she declared - it's a counter not a date
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Create 'end_date_num' for Trump dataset
just_trump_high_quality <- just_trump_high_quality |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

combined_data <- bind_rows(
  just_harris_high_quality |> mutate(candidate = "Kamala Harris"),
  just_trump_high_quality |> mutate(candidate = "Donald Trump")
)

# Fit Bayesian model with spline and pollster as fixed effect
# cf bayesian_model_1 and bayesian_model_2 where it's a random effect - note the different interpretations
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
  data = just_harris_high_quality,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the model
summary(spline_model)

# Posterior predictive checks
pp_check(spline_model)

# Fit Bayesian model with spline and pollster as a fixed effect for both Trump and Harris
combined_spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) * candidate + pollster,
  data = combined_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the model
summary(combined_spline_model)

# Posterior predictive checks
pp_check(combined_spline_model)

# Predict and plot
# Create new data for prediction
new_data <- data.frame(
  end_date_num = seq(
    min(just_harris_high_quality$end_date_num),
    max(just_harris_high_quality$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(just_harris_high_quality$pollster))
)

# Predict posterior draws
posterior_preds <- posterior_predict(spline_model, newdata = new_data)

# Summarize predictions
pred_summary <- new_data |>
  mutate(
    pred_mean = colMeans(posterior_preds),
    pred_lower = apply(posterior_preds, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds, 2, quantile, probs = 0.975)
  )

# Plot the spline fit
ggplot(just_harris_high_quality, aes(x = end_date_num, y = pct, color = pollster)) +
  geom_point() +
  geom_line(
    data = pred_summary,
    aes(x = end_date_num, y = pred_mean),
    color = "blue",
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = pred_summary,
    aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Days since earliest poll",
    y = "Percentage",
    title = "Poll Percentage over Time with Spline Fit"
  ) +
  theme_minimal()

# Create new data for prediction for both candidates
new_combined_data <- expand.grid(
  end_date_num = seq(
    min(combined_data$end_date_num),
    max(combined_data$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(combined_data$pollster)),
  candidate = c("Kamala Harris", "Donald Trump")
)

# Predict posterior draws for the combined spline model
combined_posterior_preds <- posterior_predict(combined_spline_model, newdata = new_combined_data)

# Summarize predictions
combined_pred_summary <- new_combined_data |>
  mutate(
    pred_mean = colMeans(combined_posterior_preds),
    pred_lower = apply(combined_posterior_preds, 2, quantile, probs = 0.025),
    pred_upper = apply(combined_posterior_preds, 2, quantile, probs = 0.975)
  )

# Plot the spline fit for both candidates
ggplot(combined_data, aes(x = end_date_num, y = pct, color = candidate)) +
  geom_point(aes(shape = candidate)) +
  geom_line(
    data = combined_pred_summary,
    aes(x = end_date_num, y = pred_mean, color = candidate),
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = combined_pred_summary,
    aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper, fill = candidate),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Days since earliest poll",
    y = "Percentage",
    title = "Poll Percentage over Time with Spline Fit for Trump and Harris"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



#spline fit verification
# Create new data for prediction for each candidate separately
new_data_harris_combined <- data.frame(
  end_date_num = seq(
    min(just_harris_high_quality$end_date_num),
    max(just_harris_high_quality$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(combined_data$pollster)),
  candidate = "Kamala Harris"
)

new_data_trump_combined <- data.frame(
  end_date_num = seq(
    min(just_trump_high_quality$end_date_num),
    max(just_trump_high_quality$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(combined_data$pollster)),
  candidate = "Donald Trump"
)

# Predict posterior draws for each candidate
posterior_preds_harris_combined <- posterior_predict(combined_spline_model, newdata = new_data_harris_combined)
posterior_preds_trump_combined <- posterior_predict(combined_spline_model, newdata = new_data_trump_combined)

# Summarize predictions
pred_summary_harris_combined <- new_data_harris_combined |>
  mutate(
    pred_mean = colMeans(posterior_preds_harris_combined),
    pred_lower = apply(posterior_preds_harris_combined, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds_harris_combined, 2, quantile, probs = 0.975)
  )

pred_summary_trump_combined <- new_data_trump_combined |>
  mutate(
    pred_mean = colMeans(posterior_preds_trump_combined),
    pred_lower = apply(posterior_preds_trump_combined, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds_trump_combined, 2, quantile, probs = 0.975)
  )

# Plot individual predictions from the combined model for verification
ggplot() +
  geom_point(data = just_harris_high_quality, aes(x = end_date_num, y = pct), color = "blue", alpha = 0.3) +
  geom_line(data = pred_summary_harris_combined, aes(x = end_date_num, y = pred_mean), color = "blue", linetype = "dashed") +
  geom_ribbon(data = pred_summary_harris_combined, aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper), fill = "blue", alpha = 0.2) +
  labs(title = "Kamala Harris Predictions from Combined Model", x = "Days since earliest poll", y = "Percentage") +
  theme_minimal()

ggplot() +
  geom_point(data = just_trump_high_quality, aes(x = end_date_num, y = pct), color = "red", alpha = 0.3) +
  geom_line(data = pred_summary_trump_combined, aes(x = end_date_num, y = pred_mean), color = "red", linetype = "dashed") +
  geom_ribbon(data = pred_summary_trump_combined, aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper), fill = "red", alpha = 0.2) +
  labs(title = "Donald Trump Predictions from Combined Model", x = "Days since earliest poll", y = "Percentage") +
  theme_minimal()

# Obviously this is just quick and dirty code but I hope it helps you with getting started. 
