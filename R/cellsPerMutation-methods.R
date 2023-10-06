#' Cells per mutation
#'
#' @name cellsPerMutation
#' @inherit AcidGenerics::cellsPerMutation
#' @note Updated 2023-10-06.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param minCells `integer(1)`.
#' Minimum number of cells per mutation.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' df <- cellsPerMutation(object)
#' print(df[1L:5L, 1L:5L])
NULL



## Updated 2023-10-06.
`cellsPerMutation,Cellosaurus` <- # nolint
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
        if (!any(keep)) {
            return(NULL)
        }
        mat <- mat[, keep, drop = FALSE]
        keep <- rowSums(mat) > 0L
        if (!any(keep)) {
            return(NULL)
        }
        mat <- mat[keep, , drop = FALSE]
        i <- order(colSums(mat), decreasing = TRUE)
        mat <- mat[, i, drop = FALSE]
        df1 <- as.DataFrame(mat)
        j <- rownames(df1)
        df2 <- as(object, "DFrame")
        df2 <- df2[j, .minimalCols, drop = FALSE]
        df <- cbind(df2, df1)
        df
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
        mode(mat) <- "logical"
        mat
    }



#' @rdname cellsPerMutation
#' @export
setMethod(
    f = "cellsPerMutation",
    signature = signature(object = "Cellosaurus"),
    definition = `cellsPerMutation,Cellosaurus`
)
