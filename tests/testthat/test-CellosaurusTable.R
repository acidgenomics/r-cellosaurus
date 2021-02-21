context("CellosaurusTable")

test_that("CellosaurusTable", {
    cells <- c("CVCL_0126", "CVCL_1045")
    object <- CellosaurusTable(x = cells)
    expect_s4_class(object, "CellosaurusTable")
})
