object <- Cellosaurus()

test_that("Cell name", {
    cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "22RV1" = "CVCL_1045",
        "JURKAT" = "CVCL_0065",
        "RAMOS" = "CVCL_0597"
    )
    expect_identical(cells, expected)
})

test_that("Match failure", {
    object <- mapCells(c("XXXXXX", "YYYYYY"))
    expected <- c("XXXXXX" = NA_character_, "YYYYYY" = NA_character_)
    expect_identical(object, expected)
})
