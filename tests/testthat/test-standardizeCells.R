context("standardizeCells")

test_that("Cell names", {
    cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
    expected <- c("22RV1", "JURKAT", "RAMOS")
    object <- standardizeCells(cells)
    expect_identical(object, expected)
})
