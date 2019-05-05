library(testthat)
context("Check checkers")

test_that("check_prob works", {
  
  expect_true(check_prob(1))
  expect_error(check_prob(1.5))
  expect_true(check_prob(0))

})

test_that("check_trials works", {
  
  expect_true(check_trials(1))
  expect_true(check_trials(2))
  expect_error(check_trials(-1))
})

test_that("check_success works", {
  
  expect_true(check_success(3, 5))
  expect_true(check_success(0,3))
  expect_error(check_success(5,2))
})

context("Check summary measures")

test_that("aux_mean works", {
  
  expect_equal(aux_mean(10,0.3), 3)
  expect_equal(aux_mean(5, 0.2), 1)
  expect_equal(aux_mean(1,1), 1)
})

test_that("aux_variance works", {
  
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_equal(aux_variance(5, 0.2), 0.8)
  expect_equal(aux_variance(1,1), 0)
})

test_that("aux_mode works", {
  
  expect_equal(aux_mode(10,0.3), 3)
  expect_equal(aux_mode(5, 0.2), 1)
  expect_equal(aux_mode(1,1), 2)
})

test_that("aux_skewness works", {
  
  expect_equal(aux_skewness(10,0.3), 0.2760262, tolerance = 0.02)
  expect_equal(aux_skewness(5, 0.2), 0.6708204)
  expect_equal(aux_skewness(1,1), -Inf)
})

test_that("aux_kurtosis works", {
  
  expect_equal(aux_kurtosis(10,0.3), -0.1238095, tolerance = 0.02)
  expect_equal(aux_kurtosis(5, 0.2), 0.05)
  expect_equal(aux_kurtosis(1,1), Inf)
})

context("Check binominals")

test_that("bin_choose works", {
  
  expect_equal(bin_choose(10,3), 120)
  expect_equal(bin_choose(5,2), 10)
  expect_equal(bin_choose(1,1),1)
})

test_that("bin_probability works", {
  
  expect_equal(bin_probability(4,10, 0.3), 0.2001209, tolerance = 0.02)
  expect_equal(bin_probability(4,5, 0.2), 0.0064)
  expect_equal(bin_probability(1,1,1),1)
})

test_that("bin_distribution works", {
  
  output1 <- data.frame("success" = 0:5, "probability" = c(0.16807,0.36015,0.30870,0.13230,0.02835,0.00243))
  class(output1) <- c("bindis", "data.frame")
  
  output2 <- data.frame("success" = 0:4, "probability" = c(0.6561,0.2916,0.0486,0.0036,0.0001))
  class(output2) <- c("bindis", "data.frame")
  
  output3 <- data.frame("success" = 0:1, "probability" = c(0,1))
  class(output3) <- c("bindis", "data.frame")
  
  expect_equal(bin_distribution(5, 0.3), output1)
  expect_equal(bin_distribution(4, 0.1), output2)
  expect_equal(bin_distribution(1,1), output3)
  
})

test_that("bin_cumulative works", {
  
  output1 <- data.frame("success" = 0:5, "probability" = c(0.16807,0.36015,0.30870,0.13230,0.02835,0.00243), "cumulative" = c(0.16807,0.52822,0.83692,0.96922,0.99757,1))
  class(output1) <- c("bincum", "data.frame")
  
  output2 <- data.frame("success" = 0:4, "probability" = c(0.6561,0.2916,0.0486,0.0036,0.0001), "cumulative" = c(0.6561, 0.9477,0.9963,0.9999, 1))
  class(output2) <- c("bincum", "data.frame")
  
  output3 <- data.frame("success" = 0:1, "probability" = c(0,1), "cumulative" = c(0,1))
  class(output3) <- c("bincum", "data.frame")
  
  expect_equal(bin_cumulative(5, 0.3), output1)
  expect_equal(bin_cumulative(4, 0.1), output2)
  expect_equal(bin_cumulative(1,1), output3)
  
})