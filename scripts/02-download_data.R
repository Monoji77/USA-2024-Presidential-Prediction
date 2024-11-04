#### Preamble ####
# Purpose: Linear Models for Prediction
# Author: Prankit Bhardwaj, Veyasan Ragulan, Chris Yong Hong Sen
# Date: 4 November 2024
# Contact: prankit.bhardwaj@mail.utoronto.ca 
# License: MIT



#### Workspace setup ####
library(tidyverse)

#### Download data ####
raw_elections_data <- read_csv(file = "https://projects.fivethirtyeight.com/polls/data/president_polls.csv")

#### Save data ####
write_csv(raw_elections_data, "data/01-raw_data/raw_elections_data.csv")
         
