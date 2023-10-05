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
            "A375 SKIN CJ2" = "CVCL_C8YB",
            "A375_RPMI" = NA_character_,
            "A673STAG2KO16" = NA_character_,
            "A673STAG2KO45" = NA_character_,
            "A673STAG2NT14" = NA_character_,
            "A673STAG2NT23" = NA_character_,
            "GBM001" = NA_character_,
            "HCC827GR" = NA_character_,
            "MYLA" = NA_character_,
            "PSS008" = "CVCL_C8FR",
            "PSS131R" = "CVCL_C8FU",
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
            "A375 SKIN CJ2" = "CVCL_C8YB",
            "A673STAG2KO16" = NA_character_,
            "A673STAG2KO45" = NA_character_,
            "A673STAG2NT14" = NA_character_,
            "A673STAG2NT23" = NA_character_,
            "GBM001" = NA_character_,
            "HCC827GR" = NA_character_,
            "MYLA" = NA_character_,
            "PSS008" = "CVCL_C8FR",
            "PSS131R" = "CVCL_C8FU",
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
            "A375 SKIN CJ2" = "CVCL_C8YB",
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
            "PSS008" = "CVCL_C8FR",
            "PSS131R" = "CVCL_C8FU",
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
        ## CVCL_E995 --> CVCL_1429.
        "SIDM00408",
        ## CVCL_0436 --> CVCL_H525.
        "SIDM00440",
        ## CVCL_V618 --> CVCL_1658
        "SIDM01500",
        ## CVCL_7227 --> CVCL_2037
        "SIDM01625",
        ## CVCL_VT47 --> CVCL_4511
        "SIDM01989",
        ## CVCL_7084 --> CVCL_7047
        "SIDM01997"
    )
    df <- df[setdiff(rownames(df), censor), ]
    expect_identical(
        object = unname(mapCells(object, cells = df[["model_name"]])),
        expected = df[["RRID"]]
    )
    with_collate(
        new = "C",
        code = {
            fail <- sort(map[["cmp_fail"]])
        }
    )
    expect_identical(
        object = mapCells(object, cells = fail, strict = FALSE),
        expected = c(
            "170-MG-BA" = "CVCL_LM81",
            "92-1" = "CVCL_8607",
            "A375_CJ2" = NA_character_,
            "CCC-5" = "CVCL_LM83",
            "HAP1" = "CVCL_Y019",
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
            "HEC-116" = "CVCL_2924",
            "HHUA" = "CVCL_3866",
            "HOKUG" = "CVCL_8705",
            "HOTHC" = "CVCL_8708",
            "HOTHC-SF" = "CVCL_8709",
            "HOUA-I" = "CVCL_3867",
            "HOUA-II" = "CVCL_A778",
            "HS-Os-1" = "CVCL_8716",
            "HS-PSS" = "CVCL_8717",
            "HS-Sch-2" = "CVCL_8718",
            "HSKT-C" = "CVCL_8234",
            "HSQ-89" = "CVCL_7674",
            "HTMMT" = "CVCL_8721",
            "JVE-127" = "CVCL_EG22",
            "JVE-253" = "CVCL_EG28",
            "KARPAS 1718" = "CVCL_2539",
            "KBM-7" = "CVCL_A426",
            "KP-363T" = "CVCL_EG34",
            "LO68" = "CVCL_2581",
            "MAPAC-HS-77" = "CVCL_LM84",
            "MM253" = "CVCL_2604",
            "MM485" = "CVCL_2610",
            "MOLM-1" = "CVCL_2118",
            "MUTZ-8" = "CVCL_2130",
            "MYLA" = NA_character_,
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
            "NALM-16" = "CVCL_1834",
            "NCI-H854" = "CVCL_A584",
            "NO36" = "CVCL_2658",
            "NPC268" = NA_character_,
            "NZM3" = "CVCL_D828",
            "NZOV9" = "CVCL_LJ73",
            "OCI-AML4" = "CVCL_5224",
            "OCI-Ly18" = "CVCL_1880",
            "ONE58" = "CVCL_2671",
            "P2UR_K-562" = "CVCL_L434",
            "P4E6" = "CVCL_2677",
            "PK-8" = "CVCL_4718",
            "RMS-YM" = "CVCL_A792",
            "Shmac-4" = "CVCL_2722",
            "Shmac-5" = "CVCL_2723",
            "T3M-3" = "CVCL_8065",
            "T3M-5" = "CVCL_8064",
            "TGBC18TKB" = "CVCL_3338",
            "TL-1" = "CVCL_B371",
            "TN-2" = "CVCL_5602",
            "TT1TKB" = "CVCL_8764",
            "UM-RC-3" = "CVCL_2740",
            "UM-RC-7" = "CVCL_2742",
            "UPCI-SCC-040" = "CVCL_2222",
            "UPCI-SCC-074" = "CVCL_2224",
            "UPCI-SCC-111" = "CVCL_2226",
            "UPCI-SCC-116" = "CVCL_2228",
            "UPCI-SCC-131" = "CVCL_2229",
            "UPCI-SCC-154" = "CVCL_2230",
            "UPCI-SCC-200" = "CVCL_2232",
            "WA-OSEL" = "CVCL_Y549",
            "WM3772F" = "CVCL_0B81"
        )
    )
})
