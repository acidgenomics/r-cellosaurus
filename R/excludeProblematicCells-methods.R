#' Exclude (remove) problematic and/or contaminated cell lines
#'
#' @name excludeProblematicCells
#' @note Updated 2023-08-23.
#'
#' @details
#' `excludeProblematicCells` filters based on `"Problematic cell line"` metadata
#' in cell line comments. This also removes contaminated cell lines, and is
#' more strict.
#'
#' `excludeContaminatedCells` only filters based on
#' `"Problematic cell line: Contaminated"` metadata in cell line comments. This
#' is less stringent than `excludeProblematicCells` and intentionaly keeps
#' cell lines that are misidentified but not contaminated.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Modified object, with cell lines removed.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' x <- excludeProblematicCells(object)
#' print(x)
#' y <- excludeContaminatedCells(object)
#' print(y)
NULL



## Updated 2023-08-23.
`excludeContaminatedCells,Cellosaurus` <-
    function(object) {
        assert(
            validObject(object),
            isSubset("isContaminated", colnames(object))
        )
        keep <- !object[["isContaminated"]]
        object <- object[keep, , drop = FALSE]
        object
    }



## Updated 2023-08-23.
`excludeProblematicCells,Cellosaurus` <-
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
    f = "excludeContaminatedCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeContaminatedCells,Cellosaurus`
)

#' @rdname excludeProblematicCells
#' @export
setMethod(
    f = "excludeProblematicCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeProblematicCells,Cellosaurus`
)
