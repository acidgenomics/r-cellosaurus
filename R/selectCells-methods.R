## FIXME Improve documentation about supported keys (columns).



#' @name selectCells
#' @inherit AcidGenerics::selectCells
#' @note Updated 2023-09-12.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' dim(object)
#' lineage <- colData(object)[["lineage"]][[1L]]
#' print(lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' object <- selectCells(object, lineage = lineage)
#' all(colData(object)[["lineage"]] == lineage)
#' dim(object)
NULL



## Updated 2023-09-12.
`selectCells,Cellosaurus` <-
    function(object, ...) {
        args <- list(...)
        assert(
            validObject(object),
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        colData <- colData(object)
        assert(isSubset(names(args), colnames(colData)))
        ## FIXME Only allow the user to select factor columns.
        ## Obtain the cell identifiers.
        list <- Map(
            col = names(args),
            arg = args,
            MoreArgs = list("data" = colData),
            f = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            }
        )
        cells <- sort(as.character(Reduce(f = intersect, x = list)))
        assert(hasLength(cells))
        out <- object[, cells, drop = FALSE]
        out <- droplevels2(out)
        out

    }



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "Cellosaurus"),
    definition = `selectCells,Cellosaurus`
)
