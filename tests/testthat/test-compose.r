context("Testing Compose and Sequence")

compfn <- Compose(sqrt, length, unique)
seqfn <- Sequence(sqrt, length, unique)

test_that("'Compose' and 'Sequence' are correctly orederd", {
  expect_equal(compfn(c(1:9,1:9)), 3)
  expect_equal(seqfn(c(1:9,1:9)), 18)
})
