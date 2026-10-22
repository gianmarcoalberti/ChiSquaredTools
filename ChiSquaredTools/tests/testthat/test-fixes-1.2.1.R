# Regression tests for the fixes made in version 1.2.1

# Worked example in Kvalseth (2018b, section 5.2): proportions (38) with n = 1000
kvalseth <- data.frame(
    X = factor(rep(c("x1", "x2"), each = 3)),
    Y = factor(rep(c("y1", "y2", "y3"), times = 2)),
    n = c(400, 150, 50,  50, 250, 100))

test_that("Lambda corrected reproduces Kvalseth (2018b) and uses eq. 32 for the symmetric form", {
    res <- ChiSquaredTools::chisqassoc(
        data = kvalseth, rows = "X", cols = "Y", counts = "n", rowRef = "x1", colRef = "y1",
        gkLambdaCorrected = TRUE)
    df <- res$resultsTable$asDF
    colsDep <- measureRow(df, "^Lambda corrected \\(columns dependent")
    rowsDep <- measureRow(df, "^Lambda corrected \\(rows dependent")
    sym <- measureRow(df, "^Lambda corrected \\(symmetric")
    # Y|X in the paper (columns predicted from rows): 0.3642, 95% CI [0.32, 0.41]
    expect_equal(colsDep$value, 0.3642, tolerance = 1e-3)
    expect_equal(colsDep$lowerCI, 0.3158, tolerance = 1e-3)
    expect_equal(colsDep$upperCI, 0.4127, tolerance = 1e-3)
    # X|Y in the paper: 0.40
    expect_equal(rowsDep$value, 0.4014, tolerance = 1e-3)
    # Symmetric form: the larger of the two (eq. 32), with that direction's interval
    expect_equal(sym$value, max(colsDep$value, rowsDep$value), tolerance = 1e-10)
    expect_true(sym$value >= sym$lowerCI && sym$value <= sym$upperCI)
})

test_that("Goodman-Kruskal tau labels match the variable being predicted", {
    res <- ChiSquaredTools::chisqassoc(
        data = kvalseth, rows = "X", cols = "Y", counts = "n", rowRef = "x1", colRef = "y1",
        gkTau = TRUE)
    df <- res$resultsTable$asDF
    p <- matrix(c(400, 150, 50, 50, 250, 100), 2, byrow = TRUE)
    p <- p / sum(p)
    ri <- rowSums(p)
    cj <- colSums(p)
    # rows dependent: predicting the row variable from the column variable
    tauRows <- (sum(sweep(p^2, 2, cj, "/")) - sum(ri^2)) / (1 - sum(ri^2))
    tauCols <- (sum(sweep(p^2, 1, ri, "/")) - sum(cj^2)) / (1 - sum(cj^2))
    expect_equal(measureRow(df, "^Tau \\(rows dependent")$value, tauRows, tolerance = 1e-8)
    expect_equal(measureRow(df, "^Tau \\(columns dependent")$value, tauCols, tolerance = 1e-8)
})

test_that("Yule's Q and Y share the odds ratio's p-value (2 x 2 and pairwise)", {
    res <- ChiSquaredTools::chisqassoc(
        data = twoByTwo, rows = "r", cols = "c", counts = "n", rowRef = "a", colRef = "x",
        marginFree = TRUE)
    df <- res$resultsTable$asDF
    pOR <- measureRow(df, "^Odds Ratio$")$pvalue
    expect_equal(measureRow(df, "^Yule's Q$")$pvalue, pOR, tolerance = 1e-12)
    expect_equal(measureRow(df, "^Yule's Y$")$pvalue, pOR, tolerance = 1e-12)

    res3 <- ChiSquaredTools::chisqassoc(
        data = rareRow, rows = "r", cols = "c", counts = "n", rowRef = "a", colRef = "x",
        marginFree = TRUE)
    pOR3 <- res3$pairwiseORTable$asDF$pvalue
    expect_equal(res3$pairwiseQTable$asDF$pvalue, pOR3, tolerance = 1e-12)
    expect_equal(res3$pairwiseYTable$asDF$pvalue, pOR3, tolerance = 1e-12)
})

# Azen & Walker, Table 5.1: political affiliation by age group by gender
azen51 <- data.frame(
    P = factor(rep(c("Liberal", "Moderate", "Conservative"), times = 8),
               levels = c("Liberal", "Moderate", "Conservative")),
    A = factor(rep(rep(c("18_29", "30_39", "40_49", "50_plus"), each = 3), times = 2)),
    G = factor(rep(c("Male", "Female"), each = 12), levels = c("Male", "Female")),
    n = c(27, 49, 44,  44, 54, 58,  37, 48, 49,  49, 64, 101,
          59, 54, 44,  56, 76, 55,  51, 64, 46,  63, 118, 133))

test_that("R x C x K reports the pooled chi-squared (sum of the partial tables)", {
    res <- ChiSquaredTools::chisqstrataRxC(
        data = azen51, rows = "P", cols = "A", strata = "G", counts = "n", nBootstrap = 199)
    df <- res$stratumResultsTable$asDF
    partial <- df[grepl("^Partial Table", df$stratum), ]
    pooled <- df[df$stratum == "Pooled (sum of partial)", ]
    expect_equal(nrow(pooled), 1)
    # Book values: 8.193 (males) and 24.946 (females), df = 6 each
    expect_equal(as.numeric(pooled$chisq), sum(as.numeric(partial$chisq)), tolerance = 1e-8)
    expect_equal(as.numeric(pooled$chisq), 8.193 + 24.946, tolerance = 1e-3)
    expect_equal(as.numeric(pooled$df), 12)
})
