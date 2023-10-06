test_that("cellsPerGeneFusion", {
    object <- celloFull
    df <- cellsPerGeneFusion(object)
    expect_s4_class(df, "DFrame")
    expect_null(cellsPerGeneFusion(object, minCells = nrow(object) + 1L))
})
