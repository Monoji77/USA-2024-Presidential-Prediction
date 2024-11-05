#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Prankit Bhardwaj, Veyasan Ragulan, Chris Yong Hong Sen
# Date: 4 Nov 2024
# Contact: luke.yong@mail.utoronto.ca
# License: MIT
# Pre-requisites: install testthat and tidyverse


#### Workspace setup ####
library(tidyverse)
library(testthat)

analysis_data <- read_csv("../data/02-analysis_data/analysis_data_harris.csv")


#### Test data ####
# Test that the dataset has 151 rows - there are 878  polls conducted for our desired dataset
test_that("dataset has 878  rows", {
  expect_equal(nrow(analysis_data), 878 )
})

# Test that the dataset has 3 columns
test_that("dataset has 53 columns", {
  expect_equal(ncol(analysis_data), 53)
})

# Test that the 'poll_id' column is character type
test_that("'poll_id' is double type", {
  expect_type(analysis_data$poll_id, "double")
})

# Test that the 'end_date' column is character type
test_that("end_date is date", {
  expect_true(is.Date(analysis_data$end_date))
})

# Test that the 'state' column is character type
test_that("'state' is character", {
  expect_type(analysis_data$state, "character")
})

# Test that there are no missing values in the response var pct in dataset
test_that("no missing values in pct col of dataset", {
  expect_true(all(!is.na(analysis_data$pct)))
})

# Test that 'poll_id' contains unique values (no duplicates)
test_that("'poll_id' column contains unique values", {
  expect_equal(length(unique(analysis_data$poll_id)), 465)
})

# Test that 'candidate_name' contains only kamala harris
test_that("'candidate_name' contains only kamala harris", {
  expect_true(all(analysis_data$candidate_name == 'Kamala Harris'))
})

unique(analysis_data$party)
# Test that the 'party' column contains only 'DEM' for democrat
test_that("'party' column contains at least 2 unique values", {
  expect_true(all(analysis_data$party =='DEM'))
})
