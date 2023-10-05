test_that("tnbc", {
    object <- celloFull
    expect_length(tnbc(object), 163L)
})
