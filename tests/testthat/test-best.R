# unit tests for best.R
# need to test right answers, and error conditions

# usage: test_dir("C:\\cdjProgramming\\coursera\\r\\week4\\ProgrammingAssignment3\\tests\\testthat")


source("C:\\cdjProgramming\\coursera\\r\\week4\\ProgrammingAssignment3\\best.R")

context("best.R accuracy")
test_that("best(state, outcome) returns the lowest mortality rate for the given state/outcome.", {
  expect_equal(best("TX", "heart attack"), "CYPRESS FAIRBANKS MEDICAL CENTER")
  expect_equal(best("TX", "heart failure"), "FORT DUNCAN MEDICAL CENTER")
  expect_equal(best("MD", "heart attack"), "JOHNS HOPKINS HOSPITAL, THE") 
  expect_equal(best("MD", "pneumonia"), "GREATER BALTIMORE MEDICAL CENTER") 
})

context("best.R argument validation")
test_that("best(state, outcome) returns errors for invalid states/outcomes.", {
  expect_error(best("BB", "heart attack"), "invalid state")
  expect_error(best("NY", "hert attack"), "invalid outcome")
})

context("best.R argument case insensitivity on outcome arg")
test_that("best(state, outcome) returns errors for invalid states/outcomes.", {
  expect_equal(best("NY", "heart attack"), best("NY", "heArt attack"))
  expect_equal(best("NY", "heart attack"), best("NY", "heart aTtack"))
  expect_equal(best("NY", "heart attack"), best("NY", "heArt attaCk"))
})