test_that("cellsPerGeneFusion", {
    object <- celloFull
    skip_if_not(identical(
        x = metadata(object)[["dataVersion"]],
        y = dataVersion
    ))
    df <- cellsPerGeneFusion(object)
    expect_s4_class(df, "DFrame")
    expect_identical(
        object = dim(df),
        expected = c(7666L, 105L)
    )
})
