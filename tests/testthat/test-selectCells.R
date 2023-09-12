context("Supported keys", {
    object <- celloFull
    subset <- selectCells(
        object = object,
        category = "Cancer cell line",
        organism = "Homo sapiens",
        sexOfCell = "Female"
    )
    expect_true(hasRows(subset))
    subset <- selectCells(object = object, sexOfCell = NA)
    expect_true(hasRows(subset))
})

context("Expected failures", {
    object <- celloFull
    expect_error(
        object = selectCells(object = object, XXX = "YYY"),
        regexp = "XXX"
    )
    expect_error(
        object = selectCells(object = object, category = "XXX"),
        regexp = "No cell lines"
    )
    expect_error(
        object = selectCells(object = object, category = NA),
        regexp = "No cell lines"
    )
    expect_error(
        object = selectCells(object = object, category = NULL),
        regexp = "No cell lines"
    )
    expect_error(
        object = selectCells(object = object, category = TRUE),
        regexp = "No cell lines"
    )
})
