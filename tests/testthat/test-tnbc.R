celloFull <- readRDS(file.path("cache", "celloFull.rds"))

test_that("tnbc", {
    object <- cello
    expect_identical(
        object = tnbc(object),
        expected = character()
    )
    object[1L, "comments"][[1L]][["Group"]] <-
        "Triple negative breast cancer (TNBC) cell line"
    expect_identical(
        object = tnbc(object),
        expected = rownames(object)[[1L]]
    )
    object <- celloFull
    expect_length(tnbc(object), 152L)
})
