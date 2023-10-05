test_that("Cell line name", {
    object <- cello
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("HEL", "HL60", "HMC1")
        ),
        expected = c(
            "HEL" = "CVCL_0001",
            "HL60" = "CVCL_0002",
            "HMC1" = "CVCL_0003"
        )
    )
})

test_that("Mixed identifier input", {
    object <- cello
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("CVCL_0001", "ACH-000004", "SIDM00791")
        ),
        expected = c(
            "CVCL_0001" = "CVCL_0001",
            "ACH-000004" = "CVCL_0001",
            "SIDM00791" = "CVCL_0004"
        )
    )
})

test_that("Secondary accession", {
    object <- celloFull
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("CVCL_7353", "CVCL_H249", "CVCL_V618")
        ),
        expected = c(
            "CVCL_7353" = "CVCL_0014",
            "CVCL_H249" = "CVCL_0023",
            "CVCL_V618" = "CVCL_1658"
        )
    )
})

test_that("keyType return", {
    object <- cello
    expect_identical(
        object = mapCells(
            object = object,
            cells = c("HEL", "HL-60"),
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
            cells = c("HEL", "HL-60"),
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
    expect_error(
        object = mapCells(
            object = object,
            cells = c("CVCL_0001", "XXX"),
            strict = TRUE
        ),
        regexp = "XXX"
    )
    expect_identical(
        object = mapCells(
            object = object,
            cells = "CVCL_0003",
            keyType = "depmapId",
            strict = FALSE
        ),
        expected = c("CVCL_0003" = NA_character_)
    )
    expect_error(
        object = mapCells(
            object = object,
            cells = "CVCL_0003",
            keyType = "depmapId",
            strict = TRUE
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
                "CVCL_7353",
                "UACC-62_CJ1"
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
            "CVCL_7353" = "CVCL_0014",
            "UACC-62_CJ1" = "CVCL_A1VN"
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
