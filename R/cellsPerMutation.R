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
    function(object,
             mutationFormat = c("geneName", "hgncId"),
             cellFormat = c("cellLineName", "cellosaurusId"),
             minCells = 3L) {
        assert(
            validObject(object),
            isInt(minCells)
        )
        mutationFormat <- match.arg(mutationFormat)
        cellFormat <- match.arg(cellFormat)
        cl <- mutations(object = object, format = mutationFormat)
        assert(is(cl, "CharacterList"))
        tbl <- table(cl)
        mat <- .as.matrix.table(tbl)
        type(mat) <- "logical"
        switch(
            EXPR = cellFormat,
            "cellLineName" = {
                j <- rownames(mat)
                j <- decode(object[j, "cellLineName"])
                rownames(mat) <- j
                mat <- mat[sort(rownames(mat)), , drop = FALSE]
            }
        )
        mat
    }



#' Coerce a table to matrix
#'
#' @note Updated 2023-10-05.
#' @noRd
.as.matrix.table <- # nolint
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
        mat
    }
