object <- Cellosaurus()

test_that("Cell name", {
    cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
    cells <- standardizeCells(cells)
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "22RV1" = "CVCL_1045",
        "JURKAT" = "CVCL_0065",
        "RAMOS" = "CVCL_0597"
    )
    expect_identical(cells, expected)
})

test_that("Mixed identifier input", {
    cells <- c("CVCL_0065", "ACH-000425", "SIDM00499")
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "CVCL_0065" = "CVCL_0065",
        "ACH-000425" = "CVCL_1780",
        "SIDM00499" = "CVCL_1045"
    )
    expect_identical(cells, expected)
})

test_that("keyType return", {
    cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
    cells <- standardizeCells(cells)
    expect_identical(
        object = mapCells(
            object = object,
            cells = cells,
            keyType = "depMapId"
        ),
        expected = c(
            "22RV1" = "ACH-000956",
            "JURKAT" = "ACH-000995",
            "RAMOS" = "ACH-001636"
        )
    )
    expect_identical(
        object = mapCells(
            object = object,
            cells = cells,
            keyType = "sangerId"
        ),
        expected = c(
            "22RV1" = "SIDM00499",
            "JURKAT" = "SIDM01016",
            "RAMOS" = "SIDM01094"
        )
    )
})

test_that("Match failure", {
    expect_error(
        object = mapCells(
            object = object,
            cells = c("CVCL_1045", "XXX")
        ),
        regexp = "XXX"
    )
})
