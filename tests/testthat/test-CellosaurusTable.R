context("CellosaurusTable")

test_that("CellosaurusTable", {
    ids <- c("CVCL_0126", "CVCL_1045")
    object <- CellosaurusTable(ids)
    expect_s4_class(object, "CellosaurusTable")
})
