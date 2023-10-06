test_that("mutations", {
    object <- cello
    object <- mutations(object)
    expect_s4_class(object, "CharacterList")
    expect_identical(
        object = object[["CVCL_0001"]],
        expected = c("JAK2 (6192)", "TP53 (11998)")
    )
})
