context("mapCells")

test_that("Cell name", {
    cells <- c("22RV1", "HS729", "JURKAT")
    object <- mapCells(cells, BPPARAM = BPPARAM)
    expected <- c(
        "22RV1" = "CVCL_1045",
        "HS729" = "CVCL_0871",
        "JURKAT" = "CVCL_0065"
    )
    expect_identical(object, expected)
})

test_that("Match failure", {
    object <- mapCells(c("XXXXXX", "YYYYYY"), BPPARAM = BPPARAM)
    expected <- c("XXXXXX" = NA_character_, "YYYYYY" = NA_character_)
    expect_identical(object, expected)
})
