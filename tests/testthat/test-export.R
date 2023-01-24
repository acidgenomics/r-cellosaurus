test_that("export", {
    object <- cello
    tempdir <- AcidBase::tempdir2()
    con <- file.path(tempdir, "cello.csv")
    x <- export(object = object, con = con)
    expect_true(file.exists(x))
    df <- pipette::import(con)
    expect_identical(
        object = df[1L, ],
        expected = data.frame(
            "accession" = "CVCL_0001",
            "ageAtSampling" = "30Y",
            "category" = "Cancer cell line",
            "cellLineName" = "HEL",
            "depmapId" = "ACH-000004",
            "isCancer" = TRUE,
            "isProblematic" = FALSE,
            "msiStatus" = paste(
                "Stable (MSS)",
                "(PubMed=10739008; PubMed=11226526; Sanger)"
            ),
            "ncbiTaxonomyId" = 9606L,
            "ncitDiseaseId" = "C7152",
            "ncitDiseaseName" = "Erythroleukemia",
            "organism" = "Homo sapiens",
            "population" = "Caucasian",
            "samplingSite" = "Peripheral blood",
            "sangerModelId" = "SIDM00594",
            "secondaryAccession" = NA,
            "sexOfCell" = "Male",
            "synonyms" = "Hel, GM06141, GM06141B, Human ErythroLeukemia",
            row.names = "CVCL_0001"
        )
    )
    AcidBase::unlink2(tempdir)
})
