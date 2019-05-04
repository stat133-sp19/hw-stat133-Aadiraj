library(testthat)
context("Check checkers")

test_that("check_prob with ok prob", {
  
  expect_true(check_prob(1))
  expect_false(check_prob(1.5))
  expect_true(check_prob(0))
})

test_that("check_trials with ok trials", {
  
  expect_true(check_trials(1))
  expect_true(check_trials(2))
  expect_false(check_trials(-1))
})

test_that("check_success with ok trials and success", {
  
  expect_true(check_success(3, 5))
  expect_true(check_success(0,3))
  expect_false(check_success(5,2))
})
