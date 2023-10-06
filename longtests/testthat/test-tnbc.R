test_that("tnbc", {
    object <- celloFull
    skip_if_not(identical(
        x = metadata(object)[["dataVersion"]],
        y = dataVersion
    ))
    expect_length(tnbc(object), 161L)
})
