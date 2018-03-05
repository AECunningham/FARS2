context("supplying alphabetic year should cause error")

test_that("alphabetic year causes error",{
   expect_error( make_filename(alphabetic))
})