test_that("CA runs on a 2 x J table with default settings", {
    expect_no_error(
        ChiSquaredTools::ca(data = twoByJ, rows = "r", cols = "c", counts = "n",
            geometricRefCategoryRow = "a", geometricRefCategoryCol = "w"))
})

test_that("attribute assessment is skipped, not failed, on a 2 x J table", {
    expect_no_error(
        res <- ChiSquaredTools::ca(
            data = twoByJ, rows = "r", cols = "c", counts = "n",
            geometricRefCategoryRow = "a", geometricRefCategoryCol = "w",
            showAttrInfluence = TRUE, showAttrElimination = TRUE))
    expect_equal(nrow(res$attrEliminationTable$asDF), 0)
})
