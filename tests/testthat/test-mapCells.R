context("mapCells")

test_that("Cell name", {
    cells <- c("22RV1", "JURKAT")
    object <- mapCells(cells)
    expected <- c(
        "22RV1" = "CVCL_1045",
        "JURKAT" = "CVCL_0065"
    )
    expect_identical(object, expected)
})

test_that("Match failure", {
    object <- mapCells(c("XXXXXX", "YYYYYY"))
    expected <- c("XXXXXX" = NA_character_, "YYYYYY" = NA_character_)
    expect_identical(object, expected)
})
