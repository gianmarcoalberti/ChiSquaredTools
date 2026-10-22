# Regression tests for the fixes made in version 1.2.2 (jamovi audit of 19 October 2026)

# Audit tables for the Goodman-Kruskal tau standard error
auditTau <- data.frame(
    r = factor(rep(c("a", "b"), each = 3)),
    c = factor(rep(c("x", "y", "z"), times = 2)),
    n = c(12, 3, 7,  4, 15, 6))

# Selikoff asbestos data (Beh & Lombardo 2014, Table 1.2): 5 x 4
selikoff <- data.frame(
    r = factor(rep(c("0-9", "10-19", "20-29", "30-39", "40+"), each = 4)),
    c = factor(rep(c("None", "Grade 1", "Grade 2", "Grade 3"), times = 5)),
    n = c(310, 36, 0, 0,  212, 158, 9, 0,  21, 35, 17, 4,  25, 102, 49, 18,  7, 35, 51, 28))

# Greenacre (2017) Exhibit 10.1: scientific areas x funding categories
greenacre <- data.frame(
    r = factor(rep(c("Geology", "Biochemistry", "Chemistry", "Zoology", "Physics", "Engineering",
                     "Microbiology", "Botany", "Statistics", "Mathematics"), each = 5)),
    c = factor(rep(LETTERS[1:5], times = 10)),
    n = c(3, 19, 39, 14, 10,  1, 2, 13, 1, 12,  6, 25, 49, 21, 29,  3, 15, 41, 35, 26,  10, 22, 47, 9, 26,
          3, 11, 25, 15, 34,  1, 6, 14, 5, 11,  0, 12, 34, 17, 23,  2, 5, 11, 4, 7,  2, 11, 37, 8, 20))

test_that("tau intervals use a delta-method SE on the estimate's own orientation", {
    res <- ChiSquaredTools::chisqassoc(
        data = auditTau, rows = "r", cols = "c", counts = "n", rowRef = "a", colRef = "x", gkTau = TRUE)
    df <- res$resultsTable$asDF
    colsDep <- measureRow(df, "^Tau \\(columns dependent")
    # the audit's delta-method / bootstrap SE for columns dependent is about 0.070
    se <- (colsDep$upperCI - colsDep$lowerCI) / (2 * qnorm(0.975))
    expect_equal(colsDep$value, 0.1393, tolerance = 1e-3)
    expect_equal(se, 0.070, tolerance = 0.1)
    expect_true(colsDep$lowerCI >= 0 && colsDep$upperCI <= 1)
})

test_that("CA with adjusted residuals tests independence from the Pearson decomposition", {
    tab <- data.frame(r = factor(c("a", "a", "b", "b")), c = factor(c("x", "y", "x", "y")), n = c(10, 5, 5, 10))
    res <- ChiSquaredTools::ca(data = tab, rows = "r", cols = "c", counts = "n",
                               geometricRefCategoryRow = "a", geometricRefCategoryCol = "x",
                               residualType = "adjusted", nAxes = 2)
    df <- res$summaryTable$asDF
    expect_equal(nrow(df), 2)
    expect_equal(df$statistic[1], 3.333, tolerance = 1e-3)   # Pearson chi-squared
    expect_equal(df$pvalue[1], 0.0679, tolerance = 1e-3)
    expect_equal(df$statistic[2], 13.333, tolerance = 1e-3)  # adjusted statistic, Monte Carlo p
    expect_true(df$pvalue[2] > 0.05)
    expect_equal(res$inertiaTable$asDF$malinChi[1], 3.333, tolerance = 1e-3)
})

test_that("CA reproduces Beh & Lombardo's Selikoff statistics with adjusted residuals", {
    res <- ChiSquaredTools::ca(data = selikoff, rows = "r", cols = "c", counts = "n",
                               geometricRefCategoryRow = "0-9", geometricRefCategoryCol = "None",
                               residualType = "adjusted", nAxes = 2)
    df <- res$summaryTable$asDF
    expect_equal(df$statistic[1], 648.812, tolerance = 1e-3)
    expect_equal(df$statistic[2], 1219.795, tolerance = 1e-3)
})

test_that("CA qualities never exceed 1 with adjusted residuals", {
    res <- ChiSquaredTools::ca(data = selikoff, rows = "r", cols = "c", counts = "n",
                               geometricRefCategoryRow = "0-9", geometricRefCategoryCol = "None",
                               residualType = "adjusted", nAxes = 3, showQualityContrib = TRUE)
    df <- res$qualityContribTable$asDF
    expect_true(all(df$totalQuality <= 1 + 1e-8))
    expect_equal(max(df$totalQuality), 1, tolerance = 1e-6)   # all M = 3 dims extracted
})

test_that("Cole's C7 uses the attainable maximum and equals Zysno's phi* (Ratliff's cases)", {
    # Ratliff (1982) Case 1: a = 9, b = 6, c = 0, d = 5 -> C7 = 1; audit case a=10,b=5,c=1,d=10 -> 0.785
    for (cells in list(c(9, 6, 0, 5), c(10, 5, 1, 10), c(55, 150, 200, 95))) {
        tab <- data.frame(r = factor(c("a", "a", "b", "b")), c = factor(c("x", "y", "x", "y")), n = cells)
        res <- ChiSquaredTools::chisqassoc(data = tab, rows = "r", cols = "c", counts = "n",
                                           rowRef = "a", colRef = "x", coleC7 = TRUE, zysnoPhi = TRUE)
        df <- res$resultsTable$asDF
        c7 <- measureRow(df, "^Cole's C7")$value
        expect_equal(c7, measureRow(df, "^Zysno")$value, tolerance = 1e-10)
    }
    tab <- data.frame(r = factor(c("a", "a", "b", "b")), c = factor(c("x", "y", "x", "y")), n = c(10, 5, 1, 10))
    res <- ChiSquaredTools::chisqassoc(data = tab, rows = "r", cols = "c", counts = "n",
                                       rowRef = "a", colRef = "x", coleC7 = TRUE)
    expect_equal(measureRow(res$resultsTable$asDF, "^Cole's C7")$value, 95 / 121, tolerance = 1e-6)
})

test_that("signed 2 x 2 measures follow the categories of interest", {
    tab <- data.frame(r = factor(c("a", "a", "b", "b")), c = factor(c("x", "y", "x", "y")), n = c(10, 5, 1, 10))
    r1 <- ChiSquaredTools::chisqassoc(data = tab, rows = "r", cols = "c", counts = "n",
                                      rowRef = "a", colRef = "x", phiSigned = TRUE, coleC7 = TRUE)
    r2 <- ChiSquaredTools::chisqassoc(data = tab, rows = "r", cols = "c", counts = "n",
                                      rowRef = "b", colRef = "x", phiSigned = TRUE, coleC7 = TRUE)
    expect_equal(measureRow(r1$resultsTable$asDF, "^Phi signed")$value,
                 -measureRow(r2$resultsTable$asDF, "^Phi signed")$value, tolerance = 1e-10)
    expect_equal(measureRow(r1$resultsTable$asDF, "^Cole's C7")$value,
                 -measureRow(r2$resultsTable$asDF, "^Cole's C7")$value, tolerance = 1e-10)
})

test_that("inertia gain ratio partition agrees with FactoMineR::HCPC on Greenacre's table", {
    res <- ChiSquaredTools::chisqcluster(data = greenacre, rows = "r", cols = "c", counts = "n",
                                         partitionMethod = "inertia")
    # verified against HCPC(CA(tab), min = 2, max = 9/4): 8 row clusters, 4 column clusters
    expect_equal(nrow(res$rowGroupsTable$asDF), 8)
    expect_equal(nrow(res$colGroupsTable$asDF), 4)
})

test_that("critical chi-squared clustering rejects a 2 x J table with a message", {
    expect_error(
        ChiSquaredTools::chisqcluster(data = twoByJ, rows = "r", cols = "c", counts = "n",
                                      partitionMethod = "greenacre"),
        regexp = "inertia gain ratio")
    expect_no_error(
        ChiSquaredTools::chisqcluster(data = twoByJ, rows = "r", cols = "c", counts = "n",
                                      partitionMethod = "inertia"))
})

test_that("2 x 2 x K confounding verdict does not depend on the reference category", {
    strata <- data.frame(
        r = factor(rep(c("y", "n"), times = 4)),
        c = factor(rep(rep(c("e", "u"), each = 2), times = 2)),
        s = factor(rep(c("s1", "s2"), each = 4)),
        n = c(40, 10, 20, 30,  15, 30, 35, 20))
    r1 <- ChiSquaredTools::chisqstrata2x2(data = strata, rows = "r", cols = "c", strata = "s", counts = "n",
                                          rowRef = "y", colRef = "e")
    r2 <- ChiSquaredTools::chisqstrata2x2(data = strata, rows = "r", cols = "c", strata = "s", counts = "n",
                                          rowRef = "n", colRef = "e")
    v1 <- r1$interpretationTable$asDF$result[3]
    v2 <- r2$interpretationTable$asDF$result[3]
    expect_equal(sub(":.*", "", v1), sub(":.*", "", v2))   # same scenario label
})

test_that("2 x 2 x K keeps the interpretation when only Breslow-Day-Tarone is unavailable", {
    res <- ChiSquaredTools::chisqstrata2x2(
        data = zeroCellStrata, rows = "r", cols = "c", strata = "s", counts = "n", rowRef = "y", colRef = "e")
    df <- res$interpretationTable$asDF
    expect_false(any(grepl("Could not be determined", df$result)))
    hom <- res$homogeneityTable$asDF
    expect_true(is.na(hom$statistic[2]))        # BDT blank
    expect_false(is.na(hom$statistic[1]))       # Woolf computed
})

test_that("Yule's Q and Y cut-offs are derived from OR = 2, 3, 4", {
    res <- ChiSquaredTools::chisqassoc(data = twoByTwo, rows = "r", cols = "c", counts = "n",
                                       rowRef = "a", colRef = "x", marginFree = TRUE, showThresholds = TRUE)
    df <- res$thresholdsTable$asDF
    q <- df[grepl("Yule's Q", df$measure), ]
    y <- df[grepl("Yule's Y", df$measure), ]
    expect_equal(as.numeric(q$small), (2 - 1) / (2 + 1), tolerance = 1e-3)
    expect_equal(as.numeric(y$small), (sqrt(2) - 1) / (sqrt(2) + 1), tolerance = 1e-3)
})

test_that("Monte Carlo test stops early on a table too sparse to simulate", {
    hopeless <- data.frame(r = factor(rep(paste0("r", 1:6), each = 6)), c = factor(rep(paste0("c", 1:6), times = 6)),
                           n = as.vector(diag(6)))
    t0 <- Sys.time()
    res <- ChiSquaredTools::chisqtest(data = hopeless, rows = "r", cols = "c", counts = "n",
                                      monteCarloTest = TRUE, nPerms = 999)
    expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 30)
    df <- res$testResults$asDF
    mc <- df[grepl("Monte Carlo", df$method), ]
    expect_true(grepl("0 of 999", mc$method))
    expect_true(is.na(mc$pvalue))
})
