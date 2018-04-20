library(AnomalyDetection)
context("Evaluation: AnomalyDetectionTs")

test_that("last day, both directions", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last='day')
  expect_equal(length(results$anoms), 2)
  expect_equal(length(results$anoms[[2L]]), 25)
})

test_that("both directions, e_value, with longterm", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', longterm=TRUE, e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 131)
})

test_that("both directions, e_value, threshold set to med_max", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', threshold="med_max", e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 4)
})
