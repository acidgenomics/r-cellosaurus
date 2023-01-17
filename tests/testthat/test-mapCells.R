celloFull <- readRDS(file.path("cache", "celloFull.rds"))
map <- readRDS(file.path("cache", "mapCells.rds"))

test_that("Cell name", {
    object <- cello
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
    object <- cello
    cells <- c(
        as.character(object[["id"]])[[1L]],
        as.character(object[["depmapId"]])[[1L]],
        as.character(object[["sangerModelId"]])[[4L]]
    )
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "CVCL_0001" = "CVCL_0001",
        "ACH-000004" = "CVCL_0001",
        "SIDM00791" = "CVCL_0004"
    )
    expect_identical(cells, expected)
})

test_that("keyType return", {
    object <- cello
    cells <- head(as.character(object[["name"]]), n = 2L)
    expect_identical(
        object = mapCells(
            object = object,
            cells = cells,
            keyType = "depmapId"
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
            keyType = "sangerModelId"
        ),
        expected = c(
            "HEL" = "SIDM00594",
            "HL-60" = "SIDM00829"
        )
    )
})

test_that("Match failure", {
    object <- cello
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
            keyType = "depmapId"
        ),
        regexp = "CVCL_0003"
    )
})

test_that("DepMap", {
    object <- celloFull
    df <- map[["depmap"]]
    fail <- map[["depmapFail"]]
    expect_identical(
        object = unname(mapCells(object, cells = df[[1L]])),
        expected = df[[2L]]
    )
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "12 cells"
    )
})

test_that("CellModelPassports", {
    object <- celloFull
    df <- map[["cmp"]]
    fail <- map[["cmpFail"]]
    ## "CVCL_7227" - "CVCL_2037" - F-36P
    expect_identical(
        object = unname(mapCells(object, cells = df[[1L]])),
        expected = df[[2L]]
    )
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "173 cells"
    )
})
