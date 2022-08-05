library(testthat)
testthat::test_that("check test variable in Renviron", {
                    expect_equal(as.logical(Sys.getenv("TEST_VAR")), FALSE)
                    })
