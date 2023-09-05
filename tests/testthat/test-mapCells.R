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
    expect_identical(
        object = mapCells(object, cells = fail, strict = FALSE),
        expected = c(
            "170-MG-BA" = "CVCL_LM81",
            "A375 SKIN CJ2" = NA_character_,
            "A375_RPMI" = NA_character_,
            "A673STAG2KO16" = NA_character_,
            "A673STAG2KO45" = NA_character_,
            "A673STAG2NT14" = NA_character_,
            "A673STAG2NT23" = NA_character_,
            "GBM001" = NA_character_,
            "HCC827GR" = NA_character_,
            "MYLA" = NA_character_,
            "PSS008" = NA_character_,
            "PSS131R" = NA_character_,
            "RMS-YM" = "CVCL_A792",
            "RPE1-ss111" = "CVCL_C8DM",
            "RPE1-ss119" = "CVCL_C8DN",
            "RPE1-ss48" = "CVCL_C8DQ",
            "RPE1-ss51" = "CVCL_C8DR",
            "RPE1-ss6" = "CVCL_C8DS",
            "RPE1-ss77" = "CVCL_C8DT",
            "RVH421_RPMI" = NA_character_,
            "SS1A" = NA_character_,
            "U-251 MG" = "CVCL_0021"
        )
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
    expect_identical(
        object = mapCells(object, cells = fail, strict = FALSE),
        expected = c(
            "170-MG-BA" = "CVCL_LM81",
            "A375 SKIN CJ2" = NA_character_,
            "A673STAG2KO16" = NA_character_,
            "A673STAG2KO45" = NA_character_,
            "A673STAG2NT14" = NA_character_,
            "A673STAG2NT23" = NA_character_,
            "GBM001" = NA_character_,
            "HCC827GR" = NA_character_,
            "MYLA" = NA_character_,
            "PSS008" = NA_character_,
            "PSS131R" = NA_character_,
            "RPE1-ss111" = "CVCL_C8DM",
            "RPE1-ss119" = "CVCL_C8DN",
            "RPE1-ss48" = "CVCL_C8DQ",
            "RPE1-ss51" = "CVCL_C8DR",
            "RPE1-ss6" = "CVCL_C8DS",
            "RPE1-ss77" = "CVCL_C8DT",
            "SS1A" = NA_character_
        )
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
    expect_identical(
        object = mapCells(object, cells = fail, strict = FALSE),
        expected = c(
            "A375 SKIN CJ2" = NA_character_,
            "A673STAG2KO16" = NA_character_,
            "A673STAG2KO45" = NA_character_,
            "A673STAG2NT14" = NA_character_,
            "A673STAG2NT23" = NA_character_,
            "CCLF_CORE_0002_T" = NA_character_,
            "CCLF_UPGI_0009_T" = NA_character_,
            "CCLF_UPGI_0011_T" = NA_character_,
            "CCLF_UPGI_0012_T" = NA_character_,
            "CCLF_UPGI_0015_T" = NA_character_,
            "CCLF_UPGI_0027_T" = NA_character_,
            "CCLF_UPGI_0036_T" = NA_character_,
            "CCLF_UPGI_0040_T" = NA_character_,
            "CCLF_UPGI_0041_T" = NA_character_,
            "CCLF_UPGI_0052_T" = NA_character_,
            "CCLF_UPGI_0054_T" = NA_character_,
            "CCLF_UPGI_0068_T" = NA_character_,
            "CCLF_UPGI_0085_T" = NA_character_,
            "CCLF_UPGI_0101_T" = NA_character_,
            "GBM001" = NA_character_,
            "HCC827GR" = NA_character_,
            "IPM-BO-053" = NA_character_,
            "IPM-BO-055" = NA_character_,
            "IPM-BO-056" = NA_character_,
            "MYLA" = NA_character_,
            "NH84T" = NA_character_,
            "NH93T" = NA_character_,
            "PANFR0069" = NA_character_,
            "PANFR0233" = NA_character_,
            "PANFR0368" = NA_character_,
            "PANFR0402" = NA_character_,
            "PANFR0420" = NA_character_,
            "PSS008" = NA_character_,
            "PSS131R" = NA_character_,
            "RPE1-ss111" = "CVCL_C8DM",
            "RPE1-ss119" = "CVCL_C8DN",
            "RPE1-ss48" = "CVCL_C8DQ",
            "RPE1-ss51" = "CVCL_C8DR",
            "RPE1-ss6" = "CVCL_C8DS",
            "RPE1-ss77" = "CVCL_C8DT",
            "SK-N-MM" = "CVCL_C8G1",
            "SS1A" = NA_character_
        )
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
    fail <- sort(map[["cmp_fail"]])
    expect_identical(
        object = withr::with_collate(
            new = "C",
            code = {
                mapCells(object, cells = fail, strict = FALSE),
            }
        ),
        expected = c(
            "1181N1" = "CVCL_D317",
            "1321N1" = "CVCL_0110",
            "21MT-1" = "CVCL_7931",
            "21MT-2" = "CVCL_7932",
            "21NT" = "CVCL_7933",
            "21PT" = "CVCL_7934",
            "93T449" = "CVCL_U614",
            "94T778" = "CVCL_U613",
            "950-5-BIK" = "CVCL_A1UH",
            "95T1000" = "CVCL_WU67",
            "A375_CJ1" = NA_character_,
            "A375_CJ2" = NA_character_,
            "A375_CJ3" = NA_character_,
            "AZ-521" = "CVCL_2862",
            "B239" = "CVCL_L055",
            "B240" = "CVCL_L056",
            "BB30-PBL" = "CVCL_VU86",
            "BB49-EBV" = "CVCL_VU51",
            "BB65-EBV" = "CVCL_6F66",
            "BGC-823" = "CVCL_3360",
            "BJ1-hTERT" = "CVCL_6573",
            "C17" = "CVCL_4511",
            "C666-1" = "CVCL_7949",
            "CC-LP-1" = "CVCL_0205",
            "CC-SW-1" = "CVCL_A429",
            "CCLF_PEDS_0001_T" = "CVCL_B3PK",
            "CCLF_PEDS_0003_T" = "CVCL_B3PL",
            "CCLF_PEDS_0008_T" = "CVCL_B3PM",
            "CHLA-258" = "CVCL_A058",
            "CMK-86" = "CVCL_2804",
            "COLO-357" = "CVCL_0221",
            "COLO-704" = "CVCL_1994",
            "COLO-775" = "CVCL_1996",
            "COLO-849" = "CVCL_2002",
            "COR-L51" = "CVCL_2416",
            "CP50-EBV" = "CVCL_UY79",
            "CP66-EBV" = "CVCL_VU67",
            "D-384MED" = "CVCL_1157",
            "D-556MED" = "CVCL_1165",
            "EA-hy926" = "CVCL_3901",
            "ECC2" = "CVCL_A1UI",
            "EMTOKA" = "CVCL_E115",
            "EWS-834" = "CVCL_AW95",
            "G-415" = "CVCL_8198",
            "GLC-82" = "CVCL_3371",
            "H157" = "CVCL_2458",
            "H16N2" = "CVCL_IP82",
            "H357" = "CVCL_2462",
            "H376" = "CVCL_2463",
            "H413" = "CVCL_2465",
            "HA1E" = "CVCL_VU89",
            "HA7-EBV" = "CVCL_E909",
            "HCA1" = "CVCL_2916",
            "HCC1588" = "CVCL_A351",
            "HCC1897" = "CVCL_V578",
            "HCM-SANG-0265-C18" = NA_character_,
            "HCM-SANG-0266-C20" = NA_character_,
            "HCM-SANG-0267-D12" = NA_character_,
            "HCM-SANG-0268-C18" = NA_character_,
            "HCM-SANG-0269-C18" = NA_character_,
            "HCM-SANG-0270-C20" = NA_character_,
            "HCM-SANG-0271-D12" = NA_character_,
            "HCM-SANG-0272-C20" = NA_character_,
            "HCM-SANG-0273-C18" = NA_character_,
            "HCM-SANG-0274-C18" = NA_character_,
            "HCM-SANG-0275-C18" = NA_character_,
            "HCM-SANG-0276-C18" = NA_character_,
            "HCM-SANG-0277-C18" = NA_character_,
            "HCM-SANG-0278-C20" = NA_character_,
            "HCM-SANG-0280-C18" = NA_character_,
            "HCM-SANG-0281-C18" = NA_character_,
            "HCM-SANG-0282-C18" = NA_character_,
            "HCM-SANG-0283-C18" = NA_character_,
            "HCM-SANG-0284-C18" = NA_character_,
            "HCM-SANG-0285-C18" = NA_character_,
            "HCM-SANG-0286-C20" = NA_character_,
            "HCM-SANG-0287-C20" = NA_character_,
            "HCM-SANG-0288-C18" = NA_character_,
            "HCM-SANG-0289-C15" = NA_character_,
            "HCM-SANG-0290-C15" = NA_character_,
            "HCM-SANG-0291-C15" = NA_character_,
            "HCM-SANG-0292-C15" = NA_character_,
            "HCM-SANG-0293-C15" = NA_character_,
            "HCM-SANG-0294-C15" = NA_character_,
            "HCM-SANG-0295-C15" = NA_character_,
            "HCM-SANG-0296-C15" = NA_character_,
            "HCM-SANG-0297-C15" = NA_character_,
            "HCM-SANG-0298-C15" = NA_character_,
            "HCM-SANG-0299-C15" = NA_character_,
            "HCM-SANG-0299-C15-B" = NA_character_,
            "HCM-SANG-0300-C15" = NA_character_,
            "HCM-SANG-0302-C15" = NA_character_,
            "HCM-SANG-0303-C15" = NA_character_,
            "HCM-SANG-0304-C15" = NA_character_,
            "HCM-SANG-0306-C15" = NA_character_,
            "HCM-SANG-0307-C15" = NA_character_,
            "HCM-SANG-0308-C15" = NA_character_,
            "HCM-SANG-0309-C15" = NA_character_,
            "HCM-SANG-0310-C15" = NA_character_,
            "HCM-SANG-0311-C15" = NA_character_,
            "HCM-SANG-0311-C15-B" = NA_character_,
            "HCM-SANG-0312-C15" = NA_character_,
            "HCM-SANG-0313-C15" = NA_character_,
            "HCM-SANG-0314-C15" = NA_character_,
            "HCM-SANG-0315-C25" = NA_character_,
            "HCM-SANG-0517-C20" = NA_character_,
            "HCM-SANG-0518-C18" = NA_character_,
            "HCM-SANG-0519-C20" = NA_character_,
            "HCM-SANG-0520-C18" = NA_character_,
            "HCM-SANG-0521-C18" = NA_character_,
            "HCM-SANG-0522-C18" = NA_character_,
            "HCM-SANG-0523-C15" = NA_character_,
            "HCM-SANG-0524-C20" = NA_character_,
            "HCM-SANG-0525-C18" = NA_character_,
            "HCM-SANG-0526-C15" = NA_character_,
            "HCM-SANG-0527-C18" = NA_character_,
            "HCM-SANG-0528-C25" = NA_character_,
            "HCM-SANG-0529-C18" = NA_character_,
            "HCM-SANG-0530-C15" = NA_character_,
            "HCM-SANG-0531-C25" = NA_character_,
            "HCM-SANG-0532-C20" = NA_character_,
            "HCM-SANG-0533-C20" = NA_character_,
            "HCM-SANG-0534-C18" = NA_character_,
            "HCM-SANG-0535-C20" = NA_character_,
            "HCM-SANG-0536-C18" = NA_character_,
            "HCM-SANG-0537-C18" = NA_character_,
            "HCM-SANG-0538-C15" = NA_character_,
            "HCM-SANG-0539-C18" = NA_character_,
            "HCM-SANG-0540-C25" = NA_character_,
            "HCM-SANG-0541-C20" = NA_character_,
            "HCM-SANG-0542-C18" = NA_character_,
            "HCM-SANG-0543-C15" = NA_character_,
            "HCM-SANG-0544-C15" = NA_character_,
            "HCM-SANG-0545-C15" = NA_character_,
            "HCM-SANG-0546-C15" = NA_character_,
            "HCM-SANG-0547-C25" = NA_character_,
            "HCM-SANG-0548-C25" = NA_character_,
            "HCM-SANG-0549-C15" = NA_character_,
            "HCM-SANG-0551-C18" = NA_character_,
            "HCM-SANG-0552-C25" = NA_character_,
            "HCM-SANG-0553-C25" = NA_character_,
            "HCM-SANG-0778-C25" = NA_character_,
            "HCM-SANG-0779-C25" = NA_character_,
            "HCM-SANG-0780-C25" = NA_character_,
            "HCM-SANG-1081-C20" = NA_character_,
            "HCM-SANG-1082-C15" = NA_character_,
            "HCM-SANG-1083-C16" = NA_character_,
            "HCM-SANG-1084-C15" = NA_character_,
            "HCM-SANG-1085-C20" = NA_character_,
            "HCM-SANG-1086-C18" = NA_character_,
            "HCM-SANG-1087-C18" = NA_character_,
            "HCM-SANG-1088-C18" = NA_character_,
            "HCM-SANG-1089-C18" = NA_character_,
            "HCM-SANG-1090-C18" = NA_character_,
            "HCM-SANG-1091-C18" = NA_character_,
            "HCM-SANG-1092-C18" = NA_character_,
            "HCM-SANG-1093-C18" = NA_character_,
            "HCM-SANG-1094-C16" = NA_character_,
            "HCM-SANG-1095-C25" = NA_character_,
            "HCM-SANG-1299-C15" = NA_character_,
            "HCM-SANG-1300-C18" = NA_character_,
            "HCM-SANG-1301-C20" = NA_character_,
            "HCM-SANG-1302-C15" = NA_character_,
            "HCM-SANG-1303-C18" = NA_character_,
            "HCM-SANG-1304-C20" = NA_character_,
            "HCM-SANG-1305-C15" = NA_character_,
            "HCM-SANG-1306-C25" = NA_character_,
            "HCM-SANG-1307-C25" = NA_character_,
            "HCM-SANG-1308-C25" = NA_character_,
            "HCM-SANG-1309-C25" = NA_character_,
            "HCM-SANG-1310-C15" = NA_character_,
            "HCM-SANG-1311-C18" = NA_character_,
            "HCM-SANG-1312-C20" = NA_character_,
            "HCM-SANG-1313-C18" = NA_character_,
            "HCM-SANG-1314-C18" = NA_character_,
            "HCM-SANG-1315-C18" = NA_character_,
            "HCM-SANG-1316-C18" = NA_character_,
            "HCM-SANG-1317-C20" = NA_character_,
            "HCM-SANG-1318-C20" = NA_character_,
            "HCM-SANG-1319-C18" = NA_character_,
            "HCM-SANG-1320-C25" = NA_character_,
            "HCM-SANG-1321-C18" = NA_character_,
            "HCM-SANG-1322-C15" = NA_character_,
            "HCM-SANG-1323-C15" = NA_character_,
            "HCM-SANG-1324-C18" = NA_character_,
            "HCM-SANG-1325-C15" = NA_character_,
            "HCM-SANG-1326-C15" = NA_character_,
            "HCM-SANG-1327-C18" = NA_character_,
            "HCM-SANG-1328-C15" = NA_character_,
            "HCM-SANG-1329-C15" = NA_character_,
            "HCM-SANG-1330-C19" = NA_character_,
            "HCM-SANG-1331-C18" = NA_character_,
            "HCM-SANG-1332-C18" = NA_character_,
            "HCM-SANG-1333-C20" = NA_character_,
            "HCM-SANG-1334-C18" = NA_character_,
            "HCM-SANG-1335-C18" = NA_character_,
            "HCM-SANG-1336-C15" = NA_character_,
            "HCM-SANG-1337-C18" = NA_character_,
            "HCM-SANG-1338-C20" = NA_character_,
            "HCM-SANG-1339-C18" = NA_character_,
            "HCS-2" = "CVCL_2918",
            "HCSC-1" = "CVCL_2919",
            "HEK-TE" = "CVCL_WS59",
            "HG-3" = "CVCL_Y547",
            "HK-2" = "CVCL_0302",
            "HK1" = "CVCL_7047",
            "HKGZ-CC" = "CVCL_E329",
            "HKMUS" = "CVCL_8776",
            "HLC-1" = "CVCL_5529",
            "HLF-a" = "CVCL_2255",
            "HMEL" = "CVCL_IZ34",
            "Hs-571-T" = "CVCL_0802",
            "Hs-604-T" = "CVCL_0816",
            "Hs-698-T" = "CVCL_0855",
            "Hs-706-T" = "CVCL_0863",
            "Hs-737-T" = "CVCL_0878",
            "Hs-739-T" = "CVCL_0882",
            "Hs-742-T" = "CVCL_0888",
            "Hs-751-T" = "CVCL_0891",
            "Hs-791-T" = "CVCL_S301",
            "Hs-819-T" = "CVCL_0928",
            "Hs-821-T" = "CVCL_0932",
            "Hs-822-T" = "CVCL_0933",
            "Hs-834-T" = "CVCL_0938",
            "Hs-839-T" = "CVCL_0941",
            "Hs-840-T" = "CVCL_0942",
            "Hs-860-T" = "CVCL_0957",
            "Hs-863-T" = "CVCL_0959",
            "Hs-870-T" = "CVCL_0966",
            "Hs-888-Lu" = "CVCL_0977",
            "Hs-888-Sk" = "CVCL_0978",
            "Hs-888-T" = "CVCL_0979",
            "Hs-895-Sk" = "CVCL_0992",
            "Hs-895-T" = "CVCL_0993",
            "Hs-934-T" = "CVCL_1031",
            "Hs-935-T" = "CVCL_1032",
            "Hs-936-T" = "CVCL_1033",
            "HSC-5" = "CVCL_0314",
            "HSJD-DIPG-007" = "CVCL_VU70",
            "HuG1" = "CVCL_4845",
            "HuG1-N" = "CVCL_4846",
            "HuG1-PI" = "CVCL_4847",
            "HuH-28" = "CVCL_2955",
            "HuT-102" = "CVCL_3526",
            "ICC10-6" = "CVCL_A1UZ",
            "ICC10-8" = "CVCL_A1VA",
            "ICC12" = "CVCL_A1VC",
            "ICC15" = "CVCL_A1VE",
            "ICC2" = "CVCL_VV27",
            "ICC3" = "CVCL_A1ZB",
            "ICC5" = "CVCL_VV28",
            "ICC8" = "CVCL_A1VG",
            "ICC9" = "CVCL_A1VH",
            "II-18" = "CVCL_6659",
            "IOMM-Lee" = "CVCL_5779",
            "J82-EBV" = "CVCL_5H97",
            "JH-EsoAd1" = "CVCL_8098",
            "JHC7" = "CVCL_L154",
            "JHOM-2B" = "CVCL_4645",
            "JHUEM-2" = "CVCL_4656",
            "JHUEM-3" = "CVCL_4657",
            "JK-1" = "CVCL_2079",
            "JL-1" = "CVCL_2080",
            "KARPAS-384" = "CVCL_2541",
            "KASUMI-2" = "CVCL_0590",
            "KASUMI-6" = "CVCL_0614",
            "KCI-MOH1" = "CVCL_2090",
            "KD" = "CVCL_U757",
            "KHM-1B" = "CVCL_2972",
            "KHYG-1" = "CVCL_2976",
            "KKU-M213" = "CVCL_M261",
            "KMCH-1" = "CVCL_7970",
            "KML-1" = "CVCL_2979",
            "KMLS-1" = "CVCL_2980",
            "KMRC-2" = "CVCL_2984",
            "KMRC-3" = "CVCL_2985",
            "KMS-21-BM" = "CVCL_2991",
            "KMS-28-BM" = "CVCL_2994",
            "KMS-28-PE" = "CVCL_2995",
            "KMS-28PE" = "CVCL_2995",
            "KP-1NL" = "CVCL_3003",
            "KP-MRT-RY" = "CVCL_7051",
            "L3-3" = "CVCL_8147",
            "L542" = "CVCL_VU88",
            "LB1047-EBV" = "CVCL_E908",
            "LB2241-EBV" = "CVCL_VU68",
            "LB2518-EBV" = "CVCL_E914",
            "LB373-EBV" = "CVCL_E917",
            "LB373-MEL" = "CVCL_E918",
            "LB647-PBL" = "CVCL_VU82",
            "LB771-PBL" = "CVCL_VU87",
            "LB831-EBV" = "CVCL_E913",
            "LB996-EBV" = "CVCL_E912",
            "LN-215" = "CVCL_3954",
            "LN-340" = "CVCL_3955",
            "LN-428" = "CVCL_3959",
            "LPS141" = "CVCL_M823",
            "LPS27" = "CVCL_A1VQ",
            "LPS510" = "CVCL_V414",
            "LPS6" = "CVCL_A1VR",
            "LPS853" = "CVCL_V415",
            "LS" = "CVCL_2105",
            "LS1034-PBL" = "CVCL_VV29",
            "M-07e" = "CVCL_2106",
            "MB-1" = "CVCL_2109",
            "MDA-PCa-2a" = "CVCL_4747",
            "MDA-PCa-2b" = "CVCL_4748",
            "Mel-270" = "CVCL_C302",
            "Mel-285" = "CVCL_C303",
            "Mel-290" = "CVCL_C304",
            "Mero-14" = "CVCL_2590",
            "Mero-25" = "CVCL_2591",
            "Mero-41" = "CVCL_2592",
            "Mero-48a" = "CVCL_2593",
            "Mero-48b" = "CVCL_W102",
            "Mero-48c" = "CVCL_W103",
            "Mero-48d" = "CVCL_W104",
            "Mero-82" = "CVCL_2594",
            "Mero-83" = "CVCL_2595",
            "Mero-84" = "CVCL_2596",
            "Mero-95" = "CVCL_2597",
            "Mesobank_CellLine-12" = NA_character_,
            "Mesobank_CellLine-12T" = NA_character_,
            "Mesobank_CellLine-14T" = NA_character_,
            "Mesobank_CellLine-18" = NA_character_,
            "Mesobank_CellLine-19" = NA_character_,
            "Mesobank_CellLine-2" = NA_character_,
            "Mesobank_CellLine-23T" = NA_character_,
            "Mesobank_CellLine-24" = NA_character_,
            "Mesobank_CellLine-26" = NA_character_,
            "Mesobank_CellLine-29T" = NA_character_,
            "Mesobank_CellLine-33T" = NA_character_,
            "Mesobank_CellLine-34" = NA_character_,
            "Mesobank_CellLine-35" = NA_character_,
            "Mesobank_CellLine-36" = NA_character_,
            "Mesobank_CellLine-38" = NA_character_,
            "Mesobank_CellLine-3T" = NA_character_,
            "Mesobank_CellLine-40" = NA_character_,
            "Mesobank_CellLine-45" = NA_character_,
            "Mesobank_CellLine-50T" = NA_character_,
            "Mesobank_CellLine-53T" = NA_character_,
            "Mesobank_CellLine-7T" = NA_character_,
            "MHH-CALL-3" = "CVCL_0089",
            "Mino" = "CVCL_1872",
            "MJ" = "CVCL_1414",
            "MKL-1" = "CVCL_2600",
            "MKL-2" = "CVCL_D027",
            "MM415" = "CVCL_2608",
            "MOLM-6" = "CVCL_2122",
            "MOLP-2" = "CVCL_2123",
            "MOTN-1" = "CVCL_2127",
            "MP46" = "CVCL_4D13",
            "MTA" = "CVCL_3032",
            "MUTZ-3" = "CVCL_1433",
            "MUTZ-5" = "CVCL_1873",
            "MUTZ-6" = "CVCL_L068",
            "MUTZ-7" = "CVCL_L069",
            "MYLA" = NA_character_,
            "MZ1-B" = "CVCL_VU73",
            "MZ7-B" = "CVCL_VU72",
            "NALM-1" = "CVCL_0091",
            "NALM-19" = "CVCL_1835",
            "NCC-StC-K140" = "CVCL_3055",
            "NCI-BL1184" = "CVCL_2635",
            "NCI-BL128" = "CVCL_2636",
            "NCI-BL1437" = "CVCL_2639",
            "NCI-BL1819" = "CVCL_E099",
            "NCI-BL2077" = "CVCL_E103",
            "NCI-H1184" = "CVCL_1458",
            "NCI-H1373" = "CVCL_1465",
            "NCI-H1385" = "CVCL_1466",
            "NCI-H1618" = "CVCL_1480",
            "NCI-H1819" = "CVCL_1497",
            "NCI-H1930" = "CVCL_1507",
            "NCI-H2004RT" = "CVCL_WS70",
            "NCI-H2077" = "CVCL_5157",
            "NCI-H2106" = "CVCL_1526",
            "NCI-H684" = "CVCL_9980",
            "NCI-H889" = "CVCL_1598",
            "NCO2" = "CVCL_3043",
            "NGP" = "CVCL_2141",
            "NKL" = "CVCL_0466",
            "NOZ" = "CVCL_3079",
            "NP-2" = "CVCL_1R15",
            "NP-3" = "CVCL_1R16",
            "NP-5" = "CVCL_1R17",
            "NP550" = "CVCL_B3QA",
            "NPC268" = NA_character_,
            "NPC38" = "CVCL_UH63",
            "NPC43" = "CVCL_UH64",
            "NPC53" = "CVCL_UH65",
            "NU-DHL-1" = "CVCL_1876",
            "NUGC-2" = "CVCL_1611",
            "OC-315" = "CVCL_1617",
            "OCI-Ly10" = "CVCL_8795",
            "OCI-Ly12" = "CVCL_8796",
            "OCI-Ly13-1" = "CVCL_M072",
            "OCI-Ly13-2" = "CVCL_8797",
            "OCI-Ly3" = "CVCL_8800",
            "OCI-M2" = "CVCL_2150",
            "OCI-My5" = "CVCL_E332",
            "OCUG-1" = "CVCL_3083",
            "OKF6-TERT-1" = "CVCL_L224",
            "OMM2-3" = "CVCL_C306",
            "OMM2-5" = "CVCL_C307",
            "Onda-7" = "CVCL_4W67",
            "Onda-9" = "CVCL_1R14",
            "Onda-9R" = "CVCL_4W69",
            "Onda-9R-B12" = "CVCL_4W70",
            "ONDA8" = "CVCL_4W68",
            "ORL-115" = "CVCL_S690",
            "ORL-136" = "CVCL_S691",
            "ORL-150" = "CVCL_VJ37",
            "ORL-153" = "CVCL_VJ38",
            "ORL-156" = "CVCL_VJ39",
            "ORL-166" = "CVCL_VJ40",
            "ORL-174" = "CVCL_VJ41",
            "ORL-188" = "CVCL_VJ42",
            "ORL-195" = "CVCL_VJ43",
            "ORL-204" = "CVCL_VJ45",
            "ORL-207" = "CVCL_VJ46",
            "ORL-214" = "CVCL_VJ47",
            "ORL-215" = "CVCL_VJ48",
            "ORL-48" = "CVCL_S692",
            "OS252" = "CVCL_WU66",
            "OUMS-27" = "CVCL_3090",
            "PANC-05-04" = "CVCL_1637",
            "PCM6" = "CVCL_4666",
            "PeTa" = "CVCL_LC73",
            "Pfeiffer" = "CVCL_3326",
            "PG-B95-8" = "CVCL_Y555",
            "PGA-1" = "CVCL_Y545",
            "PK-59" = "CVCL_4897",
            "PrEC-LH" = "CVCL_V626",
            "RBE" = "CVCL_4896",
            "RCC4" = "CVCL_0498",
            "RCC4-emptyvector" = "CVCL_UY81",
            "RCC4-VHL" = "CVCL_2706",
            "REC-1" = "CVCL_1884",
            "RH18DM" = NA_character_,
            "Ri-1" = "CVCL_1885",
            "RMZ" = "CVCL_8422",
            "RMZ-RC2" = "CVCL_L510",
            "RMZ-RC5" = "CVCL_L511",
            "RS-5" = "CVCL_2182",
            "SCCOHT-1" = "CVCL_VU69",
            "SF8657" = "CVCL_K085",
            "SG231" = "CVCL_0519",
            "SHI-1" = "CVCL_2191",
            "SK-RC-20" = "CVCL_V605",
            "SK-RC-31" = "CVCL_6186",
            "SKG-I" = "CVCL_2793",
            "SKG-I-TS" = "CVCL_4J75",
            "SKG-II" = "CVCL_2794",
            "SKG-II-LUC" = "CVCL_4J76",
            "SKG-II-SF" = "CVCL_8159",
            "SKG-II-TS" = "CVCL_4J77",
            "SKG-IIB" = "CVCL_VI91",
            "SKNO-1" = "CVCL_2196",
            "SLR21" = "CVCL_V607",
            "SLR25" = "CVCL_V611",
            "SMZ-1" = "CVCL_RL84",
            "SNU-119" = "CVCL_5014",
            "SNU-1544" = "CVCL_5027",
            "SNU-520" = "CVCL_5072",
            "SNU-620" = "CVCL_5079",
            "SNU-638" = "CVCL_0102",
            "Sq-1" = "CVCL_4900",
            "STM91-01" = "CVCL_8000",
            "SUM-102PT" = "CVCL_3421",
            "SUM-1315MO2" = "CVCL_5589",
            "SUM-44PE" = "CVCL_3424",
            "SUM-B002" = "CVCL_VU79",
            "SUP-T11" = "CVCL_2210",
            "SuSa" = "CVCL_L280",
            "SW1353" = "CVCL_0543",
            "T1-73" = "CVCL_3605",
            "T3M-10" = "CVCL_8067",
            "TE-125-T" = "CVCL_1740",
            "TE-14" = "CVCL_3336",
            "TE-159-T" = "CVCL_1743",
            "TE-617-T" = "CVCL_1755",
            "TIG-3" = "CVCL_E939",
            "TIG-3S" = "CVCL_3210",
            "TIG-3TD" = "CVCL_V617",
            "TKKK" = "CVCL_5599",
            "TM87-16" = "CVCL_8001",
            "TO-175-T" = "CVCL_3806",
            "Toledo" = "CVCL_3611",
            "TTC-466" = "CVCL_A444",
            "U-BLC1" = "CVCL_2738",
            "U-HO1" = "CVCL_2220",
            "U-HO1-PTPN1" = "CVCL_UI40",
            "UACC-62_CJ1" = NA_character_,
            "UM-RC-2" = "CVCL_2739",
            "UM-RC-6" = "CVCL_2741",
            "UM-UC-4" = "CVCL_2749",
            "UM-UC-5" = "CVCL_2750",
            "UPCI-SCC-016" = "CVCL_C027",
            "UPCI-SCC-026" = "CVCL_2221",
            "UT-7" = "CVCL_2233",
            "UT-7-EPO" = "CVCL_5202",
            "UT-7-GM" = "CVCL_5203",
            "UT-7-TPO" = "CVCL_5204",
            "VMRC-LCP" = "CVCL_1788",
            "VP229" = "CVCL_2754",
            "VP267" = "CVCL_2755",
            "WM88" = "CVCL_6805",
            "WPE1-NA22" = "CVCL_3810",
            "YD-10B" = "CVCL_8929",
            "YSCCC" = "CVCL_3629",
            "YSCCC-G10" = "CVCL_QZ46",
            "YSCCC-G100" = "CVCL_QZ47"
        )
    )
})
