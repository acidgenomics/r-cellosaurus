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
    ## Currently problematic matching:
    ##        ML-1        H157         RC2         CS1          K2        PL18
    ## "CVCL_0436" "CVCL_0463" "CVCL_C784" "CVCL_H245" "CVCL_6308" "CVCL_1637"
    ##         LY2        HAP1
    ## "CVCL_8799" "CVCL_5G07"
    ##
    ## Need to resolve these:
    ## "CVCL_0436" - "CVCL_H525" - ML-1
    ## "CVCL_0463" - "CVCL_2458" - NCI-H157
    ## "CVCL_C784" - "CVCL_L510" - RC-2
    ## "CVCL_H245" - "CVCL_T023" - RPMI-3460/CS1
    ## "CVCL_6308" - "CVCL_AT85" - CS-1
    ## "CVCL_1637" - "CVCL_X507" - Panc 05.04
    ## "CVCL_8799" - "CVCL_9579" - OCI-Ly2
    ## "CVCL_5G07" - "CVCL_Y019" - HAP-1
    CVCL_H525	ML-1 [Human thyroid carcinoma]	Homo sapiens (Human)
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
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "188 cells"
    )
})
