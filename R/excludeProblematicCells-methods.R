#' @name excludeProblematicCells
#' @inherit AcidGenerics::excludeProblematicCells
#' @note Updated 2023-09-12.
#'
#' @details
#' `excludeProblematicCells` filters based on `"Problematic cell line"` metadata
#' in cell line comments. This also removes contaminated cell lines, and is
#' more strict.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' print(dim(object))
#' object <- excludeProblematicCells(object)
#' print(dim(object))
NULL



## Updated 2023-08-23.
`excludeProblematicCells,Cellosaurus` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset("isProblematic", colnames(object))
        )
        keep <- !object[["isProblematic"]]
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname excludeProblematicCells
#' @export
setMethod(
    f = "excludeProblematicCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeProblematicCells,Cellosaurus`
)
