context("Testing StrsplitN")

str <- c("Lorem, ipsum, dolor", "consectetur, adipisicing, elit")

test_that("First and last element are returned", {
  firstWords <- c("Lorem", "consectetur")
  lastWords <- c("dolor", "elit")
  expect_that(strsplitN(str, ", ", 1), equals(firstWords)) 
  expect_that(strsplitN(str, ", ", 1, from="end"), equals(lastWords)) 
})

