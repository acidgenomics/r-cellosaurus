#' Cells per gene fusion
#'
#' @name cellsPerGeneFusion
#' @inherit AcidGenerics::cellsPerGeneFusion
#' @note Updated 2023-10-06.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param minCells `integer(1)`.
#' Minimum number of cells per gene fusion.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' df <- cellsPerGeneFusion(object)
#' print(df[1L:5L, 1L:5L])
NULL



## Updated 2023-10-06.
`cellsPerGeneFusion,Cellosaurus` <- # nolint
    function(object, minCells = 2L) {
        assert(validObject(object))
        cl <- geneFusions(object)
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
        j <- rownames(mat)
        df2 <- as(object, "DFrame")
        df2 <- df2[j, .minimalCols, drop = FALSE]
        df <- cbind(df2, df1)
        df
    }



#' @rdname cellsPerGeneFusion
#' @export
setMethod(
    f = "cellsPerGeneFusion",
    signature = signature(object = "Cellosaurus"),
    definition = `cellsPerGeneFusion,Cellosaurus`
)
