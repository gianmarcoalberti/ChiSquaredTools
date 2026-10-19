test_that("a priori power analysis rejects a target power not above alpha", {
    expect_error(
        ChiSquaredTools::chisqpower(data = data.frame(), analysisType = "apriori", power = 0.1, alpha = 0.2),
        "must be greater than")
})

test_that("a priori power analysis gives a sample size", {
    res <- ChiSquaredTools::chisqpower(data = data.frame(), analysisType = "apriori", power = 0.8, alpha = 0.05)
    expect_true(nrow(res$resultsTable$asDF) >= 1)
})
