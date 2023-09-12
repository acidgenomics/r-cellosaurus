#' @name excludeContaminatedCells
#' @inherit AcidGenerics::excludeContaminatedCells
#' @note Updated 2023-09-12.
#'
#' @details
#' `excludeContaminatedCells` only filters based on
#' `"Problematic cell line: Contaminated"` metadata in cell line comments. This
#' is less stringent than `excludeProblematicCells` and intentionaly keeps
#' cell lines that are misidentified but not contaminated.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' print(dim(object))
#' object <- excludeContaminatedCells(object)
#' print(dim(object))
NULL



## Updated 2023-08-23.
`excludeContaminatedCells,Cellosaurus` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset("isContaminated", colnames(object))
        )
        keep <- !object[["isContaminated"]]
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname excludeContaminatedCells
#' @export
setMethod(
    f = "excludeContaminatedCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeContaminatedCells,Cellosaurus`
)
