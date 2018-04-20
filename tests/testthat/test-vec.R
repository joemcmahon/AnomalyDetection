library(AnomalyDetection)

context("Evaluation: AnomalyDetectionVec")

test_that("last period, both directions", {
  data(raw_data)
  results <- AnomalyDetectionVec(raw_data[[2L]], max_anoms=0.02, direction='both', period=1440, only_last=TRUE)
  expect_equal(length(results), 2)
  expect_equal(length(results[[2L]]), 25)
})

test_that("both directions, e_value, with longterm", {
  data(raw_data)
  results <- AnomalyDetectionVec(raw_data[[2L]], max_anoms=0.02, direction='both', period=1440, longterm_period=1440*14, e_value=TRUE)
  expect_equal(length(results), 3)
  expect_equal(length(results[[2L]]), 131)
})

test_that("both directions, e_value, threshold set to med_max", {
  data(raw_data)
  results <- AnomalyDetectionVec(raw_data[[2L]], max_anoms=0.02, direction='both', period=1440, threshold="med_max", e_value=TRUE)
  expect_equal(length(results), 3)
  expect_equal(length(results[[2L]]), 6)
})
