#' Cells per gene fusion
#'
#' @export
#' @note Updated 2023-10-06.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param minCells `integer(1)`.
#' Minimum number of cells per gene fusion.
#'
#' @return `DFrame`.
#' Gene fusion pairs in columns, cells in rows.
#' Includes additional cell line metadata on left side.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' df <- cellsPerGeneFusion(object)
#' print(df[1L:5L, 1L:5L])
cellsPerGeneFusion <-
    function(object, minCells = 2L) {
        assert(validObject(object))
        cl <- geneFusions(object)
        assert(is(cl, "CharacterList"))
        tbl <- table(cl)
        mat <- .tableToLogicalMatrix(tbl)
        keep <- colSums(mat) >= minCells
        mat <- mat[, keep, drop = FALSE]
        keep <- rowSums(mat) > 0L
        mat <- mat[keep, , drop = FALSE]
        i <- order(colSums(mat), decreasing = TRUE)
        mat <- mat[, i, drop = FALSE]
        df1 <- as(mat, "DFrame")
        j <- rownames(df1)
        df2 <- as(object, "DFrame")
        df2 <- df2[j, .minimalCols, drop = FALSE]
        df <- cbind(df2, df1)
        df
    }
