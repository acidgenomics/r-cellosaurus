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
            "depMapId",
            "derivedFrom",
            "id",
            "isCancer",
            "isProblematic",
            "name",
            "ncbiTaxonomyId",
            "ncitDiseaseId",
            "ncitDiseaseName",
            "organism",
            "originateFromSameIndividualAs",
            "sangerId",
            "sex",
            "synonym"
        )
    )
    AcidBase::unlink2(tempdir)
})