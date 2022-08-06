library(testthat)
testthat::test_that("check test variable in Renviron", {
                    expect_equal(test_var, FALSE)
                    })
