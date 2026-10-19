test_that("corrected measures run when a bootstrap replicate loses a rare row", {
    expect_no_error(
        res <- ChiSquaredTools::chisqassoc(
            data = rareRow, rows = "r", cols = "c", counts = "n", rowRef = levels(rareRow$r)[1], colRef = levels(rareRow$c)[1],
            phiCorrected = TRUE, cAdjusted = TRUE, cCorrected = TRUE, vCorrected = TRUE,
            wHat = TRUE, wHatCorrected = TRUE, sakoda = TRUE, sakodaCorrected = TRUE,
            bootstrapReps = 999, seed = 1))
    df <- res$resultsTable$asDF
    v <- measureRow(df, "V corrected")
    expect_true(is.finite(v$lowerCI) && is.finite(v$upperCI))
})

test_that("noncentrality-based estimate lies within its confidence interval (df = 81)", {
    res <- ChiSquaredTools::chisqassoc(
        data = largeDf, rows = "r", cols = "c", counts = "n", rowRef = levels(largeDf$r)[1], colRef = levels(largeDf$c)[1],
        contingencyC = TRUE, cramersV = TRUE, cohenW = TRUE)
    df <- res$resultsTable$asDF
    for (m in c("Contingency Coefficient", "^Cramér's V$", "Cohen's w")) {
        row <- measureRow(df, m)
        expect_gte(row$valueNcp, row$lowerCI - 1e-10)
        expect_lte(row$valueNcp, row$upperCI + 1e-10)
    }
})

test_that("bias-corrected Cramer's V follows Bergsma (2013)", {
    tab <- matrix(c(8, 3, 2, 7), 2, 2)
    n <- sum(tab); r <- 2; k <- 2
    chi2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
    phi2 <- max(0, chi2 / n - (r - 1) * (k - 1) / (n - 1))
    rt <- r - (r - 1)^2 / (n - 1); kt <- k - (k - 1)^2 / (n - 1)
    expected <- unname(sqrt(phi2 / min(rt - 1, kt - 1)))
    res <- ChiSquaredTools::chisqassoc(
        data = twoByTwo, rows = "r", cols = "c", counts = "n", rowRef = levels(twoByTwo$r)[1], colRef = levels(twoByTwo$c)[1], vBiasCorrected = TRUE)
    row <- measureRow(res$resultsTable$asDF, "bias-corrected")
    expect_equal(row$value, expected, tolerance = 1e-8)
})
