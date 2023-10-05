#' Cells per gene fusion
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @inheritParams geneFusions
#'
#' @return `matrix`.
#' Logical matrix.
#' Gene fusion pairs in columns, cells in rows.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' mat <- cellsPerGeneFusion(object)
#' print(mat[1L:5L, 1L:5L])
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
        j <- rownames(mat)
        rn <- paste0(decode(object[j, "cellLineName"]), " (", j, ")")
        rownames(mat) <- rn
        mat
    }
