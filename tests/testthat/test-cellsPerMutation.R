test_that("cellsPerMutation", {
    object <- cello
    df <- cellsPerMutation(object)
    expect_s4_class(df, "DFrame")
    expect_null(cellsPerMutation(object, minCells = nrow(object) + 1L))
})
