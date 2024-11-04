#### Preamble ####
# Purpose: Simulates sampling methodology from a patriot pollster.
# Author: Rohan Alexander
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)
library(readxl)

#### Simulate data ####
# obtain actual population data per county of wisconsin, include proportion of population
wisconsin_population_data <- read_csv("../data/00-simulated_data/wisconsin_population_data.csv") |>
  select(county = name,
         population = pop2024) |>
  mutate(weight = population / sum(population)) |>
  mutate(county = case_when(startsWith(county, 'Green Lake') ~ 'green lake',
                            .default=tolower(str_extract(county, '^[^ ]+'))),
         county = gsub('\\.', '', county))

# obtain county code to identify county
county_code <- read_xls("../data/00-simulated_data/icpsrcnt.xls") |>
  janitor::clean_names() |>
  select(state:county)

# read in census data for wisconsin
census_data <- read_csv("../data/00-simulated_data/usa_00004.csv") |>
  janitor::clean_names() |>
  filter(stateicp ==25 & cismrtphn != 0 & age >= 18 & educ <= 11) |>
  inner_join(county_code,
             by = c('stateicp'='stateicp',
                    'countyicp'='county_cod')) |>
  select(county, has_smartphone = cismrtphn, age, race) |>
  mutate(county = tolower(county), 
         has_smartphone = ifelse(has_smartphone==2, 0, has_smartphone),
         race = case_when(
           race == 1 ~ 'White',
           race == 2 ~ 'Black',
           race %in% c(4, 5, 6) ~ 'Asian',
           .default = 'others'),
         age_bin = case_when(age <= 30 ~ '18-30',
                             age <= 65 ~ '30-65',
                             .default = 'above 65')) |>
  inner_join(wisconsin_population_data,
             by = c('county'='county')) |>
  arrange(age)

#### Save data ####
write_csv(census_data, "../data/00-simulated_data/simulated_appendix_data.csv")


