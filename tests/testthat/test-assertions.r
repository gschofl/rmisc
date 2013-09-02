context("Testing Assertions")

test_that("'is.scalar' returns TRUE for objects of length 1", {
  expect_true(is.scalar(1))
  expect_true(is.scalar(""))
  expect_true(is.scalar(list(1)))
  
  expect_false(is.scalar(NULL))
  expect_false(is.scalar(integer(2)))
  expect_false(is.scalar(integer()))
})

test_that("'is.empty' returns TRUE on empty vectors of length one", {
  expect_that(is.empty(NULL), is_true())
  expect_that(is.empty(""), is_true())
  expect_that(is.empty(numeric()), is_true())
  
  expect_that(is.empty(list(NULL)), is_false())
  expect_that(is.empty(c("","")), is_false())
})

test_that("'are_empty' returns a logical vector", {
  expect_that(are_empty(NULL), is_true())
  expect_that(are_empty(""), is_true())
  expect_that(are_empty(numeric()), is_true())
  
  expect_that(are_empty(list(NULL)), is_true())
  expect_that(are_empty(list(NULL, NULL)), equals(c(TRUE,TRUE)))
  expect_that(are_empty(c("","")), equals(c(TRUE,TRUE)))
  expect_that(are_empty(list(NA,NULL,"",1)), equals(c(FALSE,TRUE,TRUE,FALSE)))
})

test_that("'all_empty' returns TRUE on empty vectors of all lengths", {
  expect_that(all_empty(NULL), is_true())
  expect_that(all_empty(""), is_true())
  expect_that(all_empty(numeric()), is_true())
  
  expect_that(all_empty(list(NULL, NULL)), is_true())
  expect_that(all_empty(c("","")), is_true())
})

test_that("'has_command' returns TRUE on executables that exist", {
  expect_true(has_command("ftp"))
  expect_false(has_command("does-not-exist"))
  expect_that(assert_that(has_command("does-not-exist")), throws_error())
})

test_that("'is.installed' returns TRUE if a package is installed", {
  expect_true(is.installed("methods"))
  expect_false(is.installed(""))
})



