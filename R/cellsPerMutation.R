#' Cells per mutation
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @inheritParams mutations
#'
#' @return `matrix`.
#' Logical matrix.
#' Genes in columns, cells in rows.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' mat <- cellsPerMutation(object)
#' print(mat[1L:5L, 1L:5L])
cellsPerMutation <-
    function(object, minCells = 2L) {
        assert(
            validObject(object),
            isInt(minCells)
        )
        cl <- mutations(object)
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



#' Coerce a table to logical matrix
#'
#' @note Updated 2023-10-05.
#' @noRd
.tableToLogicalMatrix <- # nolint
    function(object) {
        assert(is(object, "table"))
        mat <- matrix(
            data = unclass(object),
            nrow = nrow(object),
            ncol = ncol(object),
            byrow = FALSE,
            dimnames = dimnames(object)
        )
        assert(
            identical(rowSums(object), rowSums(object)),
            identical(colSums(object), colSums(object))
        )
        type(mat) <- "logical"
        mat
    }
