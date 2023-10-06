test_that("geneFusions", {
    object <- cello
    object <- geneFusions(object)
    expect_s4_class(object, "CharacterList")
    expect_identical(
        object = object[["CVCL_0004"]],
        expected = "BCR-ABL1 (76-1014)"
    )
})
