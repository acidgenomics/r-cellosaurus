object <- cello

test_that("Cell name", {
    cells <- head(object[["name"]], n = 3L)
    cells <- standardizeCells(cells)
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "HEL" = "CVCL_0001",
        "HL60" = "CVCL_0002",
        "HMC1" = "CVCL_0003"
    )
    expect_identical(cells, expected)
})

test_that("Mixed identifier input", {
    cells <- c(
        object[["id"]][[1L]],
        object[["depMapId"]][[2L]],
        object[["sangerId"]][[4L]]
    )
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "CVCL_0001" = "CVCL_0001",
        "ACH-000002" = "CVCL_0002",
        "SIDM00791" = "CVCL_0004"
    )
    expect_identical(cells, expected)
})

test_that("keyType return", {
    cells <- head(object[["name"]], n = 2L)
    expect_identical(
        object = mapCells(
            object = object,
            cells = cells,
            keyType = "depMapId"
        ),
        expected = c(
            "HEL" = "ACH-000004",
            "HL-60" = "ACH-000002"
        )
    )
    expect_identical(
        object = mapCells(
            object = object,
            cells = cells,
            keyType = "sangerId"
        ),
        expected = c(
            "HEL" = "SIDM00594",
            "HL-60" = "SIDM00829"
        )
    )
})

test_that("Match failure", {
    expect_error(
        object = mapCells(
            object = object,
            cells = c("CVCL_0001", "XXX")
        ),
        regexp = "XXX"
    )
    expect_error(
        object = mapCells(
            object = object,
            cells = "CVCL_0003",
            keyType = "depMapId"
        ),
        regexp = "CVCL_0003"
    )
})
