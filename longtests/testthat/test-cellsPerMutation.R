test_that("cellsPerMutation", {
    object <- celloFull
    skip_if_not(identical(
        x = metadata(object)[["dataVersion"]],
        y = dataVersion
    ))
    df <- cellsPerMutation(object)
    expect_s4_class(df, "DFrame")
    expect_identical(
        object = dim(df),
        expected = c(17247L, 200L)
    )
})
