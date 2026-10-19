test_that("Monte Carlo test runs on a sparse table and gives a valid p-value", {
    expect_no_error(
        res <- ChiSquaredTools::chisqtest(
            data = sparseRow, rows = "r", cols = "c", counts = "n",
            monteCarloTest = TRUE, permTest = TRUE, nPerms = 499, seed = 1))
    df <- res$testResults$asDF
    p <- df$pvalue[grepl("Monte Carlo", df$method)]
    expect_true(length(p) == 1 && p > 0 && p <= 1)
})
