celloFull <- readRDS(file.path("cache", "celloFull.rds"))
map <- readRDS(file.path("cache", "mapCells.rds"))

test_that("Cell name", {
    object <- cello
    cells <- head(object[["cellLineName"]], n = 3L)
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
        as.character(object[["accession"]])[[1L]],
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
    cells <- head(as.character(object[["cellLineName"]]), n = 2L)
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
                "U87MG",
                "CHL-1-DM",
                "D384",
                "NCI-H157-DM",
                "OCILY-13",
                "CVCL_7353"
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
            "U87MG" = "CVCL_0022",
            "CHL-1-DM" = "CVCL_1122",
            "D384" = "CVCL_1157",
            "NCI-H157-DM" = "CVCL_0463",
            "OCILY-13" = "CVCL_8797",
            "CVCL_7353" = "CVCL_0014"
        )
    )
})

## FIXME Failed to map 3 cells: TM-87, SU-MB-002, PC-3_[JPC-3].

test_that("DepMap 22Q2", {
    object <- celloFull
    df <- map[["depmap_22q2"]]
    censor <- c(
        "ACH_000499",
        "ACH_001131",
        "ACH_002185"
    )
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["cell_line_name"]])),
        expected = df[["RRID"]]
    )
    fail <- map[["depmap_22q2_fail"]]
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "19 cells"
    )
})

## FIXME Failed to map 2 cells: TM-87, PC-3_[JPC-3].

test_that("DepMap 22Q4", {
    object <- celloFull
    df <- map[["depmap_22q4"]]
    censor <- "ACH_002185"
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["CellLineName"]])),
        expected = df[["RRID"]]
    )
    fail <- map[["depmap_22q4_fail"]]
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "12 cells"
    )
})

## FIXME Failed to map 4 cells: PE-CA-PJ34_Clone-C12,
## PE-CA-PJ41_Clone-D2, PC-3_[JPC-3], NTERA-2-cl-D1.

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
        "SIDM00122",
        "SIDM00408",
        "SIDM00440",
        "SIDM00912",
        "SIDM01335",
        "SIDM01500"
    )
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["model_name"]])),
        expected = df[["RRID"]]
    )
    fail <- map[["cmp_fail"]]
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "173 cells"
    )
})
