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
            "comment",
            "creationDate",
            "depMapId",
            "derivedFrom",
            "id",
            "isCancer",
            "isProblematic",
            "name",
            "ncbiTaxonomyId",
            "ncitDiseaseId",
            "ncitDiseaseName",
            "obsolete",
            "organism",
            "originateFromSameIndividualAs",
            "sangerId",
            "sex",
            "subset",
            "synonym",
            "xref"
        )
    )
    AcidBase::unlink2(tempdir)
})
