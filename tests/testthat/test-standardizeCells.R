test_that("Cell names", {
    object <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
    expected <- c("22RV1", "JURKAT", "RAMOS")
    object <- standardizeCells(object)
    expect_identical(object, expected)
})
