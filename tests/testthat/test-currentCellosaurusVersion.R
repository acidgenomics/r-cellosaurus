test_that("currentCellosaurusVersion", {
    expect_s3_class(
        object = currentCellosaurusVersion(),
        class = "numeric_version"
    )
})
