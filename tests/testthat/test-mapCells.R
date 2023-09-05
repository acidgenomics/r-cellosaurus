## FIXME Instead of error, check for NA values.

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

test_that("Secondary accession", {
    object <- celloFull
    cells <- c("CVCL_7353", "CVCL_H249")
    cells <- mapCells(object = object, cells = cells)
    expected <- c(
        "CVCL_7353" = "CVCL_0014",
        "CVCL_H249" = "CVCL_0023"
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
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("CVCL_0001", "XXX"),
            strict = FALSE
        ),
        expected = c(
            "CVCL_0001" = "CVCL_0001",
            "XXX" = NA_character_
        )
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

test_that("ATCC identifiers and misspellings", {
    object <- celloFull
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("Hs 578T", "HTB-126", "HTB126")
        ),
        expected = c(
            "Hs 578T" = "CVCL_0332",
            "HTB-126" = "CVCL_0332",
            "HTB126" = "CVCL_0332"
        )
    )
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("CCL-228", "CRL-3038", "HTB-126")
        ),
        expected = c(
            "CCL-228" = "CVCL_0546",
            "CRL-3038" = "CVCL_AQ42",
            "HTB-126" = "CVCL_0332"
        )
    )
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("Duadi", "CANPAN-2")
        ),
        expected = c(
            "Duadi" = "CVCL_0008",
            "CANPAN-2" = "CVCL_0026"
        )
    )
})

test_that("DepMap 22Q2", {
    object <- celloFull
    df <- map[["depmap_22q2"]]
    censor <- c("ACH_000499", "ACH_001131", "ACH_002185")
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["cell_line_name"]])),
        expected = df[["RRID"]]
    )
    fail <- sort(map[["depmap_22q2_fail"]])
    ## Mapping failures:
    ## A375 SKIN CJ2, A375_RPMI, A673STAG2KO16, A673STAG2KO45, A673STAG2NT14,
    ## A673STAG2NT23, GBM001, HCC827GR, MYLA, PSS008, PSS131R, RVH421_RPMI,
    ## SS1A.
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "13 cells"
    )
})

test_that("DepMap 22Q4", {
    object <- celloFull
    df <- map[["depmap_22q4"]]
    censor <- c("ACH_000499", "ACH_001131", "ACH_002185")
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["CellLineName"]])),
        expected = df[["RRID"]]
    )
    fail <- sort(map[["depmap_22q4_fail"]])
    ## Mapping failures:
    ## A375 SKIN CJ2, A673STAG2KO16, A673STAG2KO45, A673STAG2NT14,
    ## A673STAG2NT23, GBM001, HCC827GR, MYLA, PSS008, PSS131R, SS1A.
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "11 cells"
    )
})

test_that("DepMap 23Q2", {
    object <- celloFull
    df <- map[["depmap_23q2"]]
    censor <- c("ACH_000499", "ACH_001131", "ACH_002185")
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["CellLineName"]])),
        expected = df[["RRID"]]
    )
    fail <- sort(map[["depmap_23q2_fail"]])
    ## Mapping failures:
    ## A375 SKIN CJ2, A673STAG2KO16, A673STAG2KO45, A673STAG2NT14,
    ## A673STAG2NT23, CCLF_CORE_0002_T, CCLF_UPGI_0009_T, CCLF_UPGI_0011_T,
    ## CCLF_UPGI_0012_T, CCLF_UPGI_0015_T, CCLF_UPGI_0027_T, CCLF_UPGI_0036_T,
    ## CCLF_UPGI_0040_T, CCLF_UPGI_0041_T, CCLF_UPGI_0052_T, CCLF_UPGI_0054_T,
    ## CCLF_UPGI_0068_T, CCLF_UPGI_0085_T, CCLF_UPGI_0101_T, GBM001, HCC827GR,
    ## IPM-BO-053, IPM-BO-055, IPM-BO-056, MYLA, NH84T, NH93T, PANFR0069,
    ## PANFR0233, PANFR0368, PANFR0402, PANFR0420, PSS008, PSS131R, SS1A.
    expect_error(
        object = mapCells(object, cells = fail),
        regexp = "35 cells"
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
