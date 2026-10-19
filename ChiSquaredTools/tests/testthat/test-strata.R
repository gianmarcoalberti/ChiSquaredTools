test_that("2 x 2 x K runs when the common odds ratio is not finite", {
    expect_no_error(
        res <- ChiSquaredTools::chisqstrata2x2(
            data = zeroCellStrata, rows = "r", cols = "c", strata = "s", counts = "n",
            rowRef = "y", colRef = "e"))
    expect_equal(nrow(res$cmhTestTable$asDF), 1)
})

test_that("R x C x K bootstrap runs with a rare row", {
    expect_no_error(
        ChiSquaredTools::chisqstrataRxC(
            data = rareRowStrata, rows = "a", cols = "b", strata = "s", counts = "n",
            nBootstrap = 199))
})
