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
  
  #tests from the spec
  expect_true(is.na(rankhospital("MN", "heart attack", 5000)))
  expect_equal(rankhospital("MD", "heart attack", "worst"), "HARFORD MEMORIAL HOSPITAL")
  expect_equal(rankhospital("TX", "heart failure", 4), "DETAR HOSPITAL NAVARRO")
})

context("rankhospital.R - multiple rank")
test_that("rankhospital(state, outcome) returns the hospital(s) at the given rank for the given outcome", {
  expect_true(is.na(rankhospital("WA", "heart attack", num=1.5:3)))
  expect_equal(rankhospital("WA", "heart attack", num=3:5), c("PEACEHEALTH ST JOSEPH MEDICAL CENTER", "VIRGINIA MASON MEDICAL CENTER", "PEACHEALTH ST JOHN MEDICAL CENTER"))
  expect_equal(rankhospital("WA", "heart attack", num=5:3), c("PEACHEALTH ST JOHN MEDICAL CENTER", "VIRGINIA MASON MEDICAL CENTER", "PEACEHEALTH ST JOSEPH MEDICAL CENTER"))
  # interesting it looks like there's a typo in the data - discovered by unit tests!
  })

