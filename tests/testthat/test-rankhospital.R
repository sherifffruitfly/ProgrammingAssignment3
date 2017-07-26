# unit tests for rankhospital.R
# need to test right answers, and error conditions, argument validation

# usage: test_dir("C:\\cdjProgramming\\coursera\\r\\week4\\ProgrammingAssignment3\\tests\\testthat")
# usage: test_file("C:\\cdjProgramming\\coursera\\r\\week4\\ProgrammingAssignment3\\tests\\testthat\\test-rankhospital.R")

source("C:\\cdjProgramming\\coursera\\r\\week4\\ProgrammingAssignment3\\rankhospital.R")

context("rankhospital.R - single rank")
test_that("rankhospital(state, outcome) returns the hospital(s) at the given rank for the given outcome", {
  expect_true(is.na(rankhospital("WA", "heart attack", num=650)))
  expect_true(is.na(rankhospital("WA", "heart attack", num=-8)))
  expect_true(is.na(rankhospital("WA", "heart attack", num=1.3)))
  expect_equal(rankhospital("WA", "heart attack", num=2), "SWEDISH MEDICAL CENTER/CHERRY HILL")
  expect_equal(rankhospital("WA", "pneumonia", num="best"), "EVERGREEN HOSPITAL MEDICAL CENTER")
  expect_equal(rankhospital("WA", "heart failure", num="worst"), "MID VALLEY HOSPITAL")
  expect_true(is.na(rankhospital("WA", "heart attack", num="pie")))
})

context("rankhospital.R - multiple rank")
test_that("rankhospital(state, outcome) returns the hospital(s) at the given rank for the given outcome", {
  expect_true(is.na(rankhospital("WA", "heart attack", num=1.5:3)))
})

