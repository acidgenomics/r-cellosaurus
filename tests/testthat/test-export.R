test_that("export", {
    object <- cello
    tempdir <- AcidBase::tempdir2()
    con <- file.path(tempdir, "cello.csv")
    x <- export(object = object, con = con)
    expect_true(file.exists(x))
    df <- pipette::import(con)
    expect_identical(
        object = colnames(df),
        expected = c(
            "accession",
            "ageAtSampling",
            "category",
            "cellLineName",
            "depmapId",
            "isCancer",
            "isProblematic",
            "msiStatus",
            "ncbiTaxonomyId",
            "ncitDiseaseId",
            "ncitDiseaseName",
            "organism",
            "population",
            "samplingSite",
            "sangerModelId",
            "secondaryAccession",
            "sexOfCell",
            "synonyms"
        )
    )
    AcidBase::unlink2(tempdir)
})
