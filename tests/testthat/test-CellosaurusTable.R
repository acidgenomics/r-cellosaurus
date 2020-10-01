context("CellosaurusTable")

test_that("CellosaurusTable", {
    ids <- c("CVCL_0126", "CVCL_1045")
    object <- CellosaurusTable(x = ids, BPPARAM = BPPARAM)
    expect_s4_class(object, "CellosaurusTable")
})
