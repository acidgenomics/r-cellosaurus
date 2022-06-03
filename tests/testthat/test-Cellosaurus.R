test_that("Cellosaurus", {
    object <- Cellosaurus()
    expect_s4_class(object, "CellosaurusTable")
})
