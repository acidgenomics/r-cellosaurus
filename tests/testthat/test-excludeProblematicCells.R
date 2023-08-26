## Cover both `excludeContaminatedCells` and `excludeProblematicCells`.

test_that("excludeProblematicCells", {
    for (var in c("contaminated", "problematic")) {
        object <- cello
        colName <- camelCase(paste("is", var))
        funName <- camelCase(paste("exclude", var, "cells"))
        fun <- get(funName)
        vals <- Rle(c(TRUE, rep(FALSE, nrow(object) - 1L)))
        object[colName] <- vals
        object2 <- fun(object)
        expect_identical(
            object = nrow(object) - nrow(object2),
            expected = 1L
        )
    }
})
