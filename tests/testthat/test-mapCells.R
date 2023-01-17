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
    censor <- "ACH_002185" # PL18
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["CellLineName"]])),
        expected = df[["RRID"]]
    )
    fail <- map[["depmapFail"]]
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "12 cells"
    )
})

test_that("CellModelPassports", {
    object <- celloFull
    df <- map[["cmp"]]
    df[["RRID"]] <- gsub(
        pattern = "CVCL_2717;CVCL_1888",
        replacement = "CVCL_1888",
        x = df[["RRID"]]
    )
    df[["RRID"]] <- gsub(
        pattern = "CVCL_X507",
        replacement = "CVCL_1637",
        x = df[["RRID"]]
    )
    censor <- c(
        "SIDM00408", # MS-1; CVCL_E995
        "SIDM00440", # ML-1; CVCL_0436
        "SIDM00912", # COLO-699N; CVCL_1992s
        "SIDM01335", # SNU-1272; CVCL_5020
        "SIDM01500" # EW-8; CVCL_V618
    )
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["model_name"]])),
        expected = df[["RRID"]]
    )
    fail <- map[["cmpFail"]]
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "173 cells"
    )
})

test_that("Tricky cell lines", {
    object <- celloFull
    expect_identical(
        object = mapCells(
            object = object,
            cells = c(
                "A7",
                "BE2C",
                "DOTC24510",
                "G292CLONEA141B1",
                "HS729",
                "HS936TC1",
                "MCIXC",
                "NTERA2CLD1",
                "OCUG1",
                "RAMOSRA1",
                "SJRH30",
                "U87MG"
            )
        ),
        expected = c(
            "A7" = "CVCL_3486",
            "BE2C" = "CVCL_0529",
            "DOTC24510" = "CVCL_1181",
            "G292CLONEA141B1" = "CVCL_2909",
            "HS729" = "CVCL_0871",
            "HS936TC1" = "CVCL_1034",
            "MCIXC" = "CVCL_1398",
            "NTERA2CLD1" = "CVCL_3407",
            "OCUG1" = "CVCL_3083",
            "RAMOSRA1" = "CVCL_0597",
            "SJRH30" = "CVCL_0041",
            "U87MG" = "CVCL_0022"
        )
    )
})
