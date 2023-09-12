## FIXME Move to AcidGenerics.

#' @export
#' @rdname excludeProblematicCells
setGeneric(
    name = "excludeContaminatedCells",
    def = function(object, ...) {
        standardGeneric("excludeContaminatedCells")
    }
)

## FIXME Move to AcidGenerics.

#' @export
#' @rdname excludeProblematicCells
setGeneric(
    name = "excludeProblematicCells",
    def = function(object, ...) {
        standardGeneric("excludeProblematicCells")
    }
)

#' @export
#' @name export
#' @usage export(object, con, format, ...)
NULL

#' @export
#' @name mapCells
#' @usage mapCells(object, ...)
NULL

#' @export
#' @name selectCells
#' @usage selectCells(object, ...)
NULL

#' @export
#' @name standardizeCells
#' @usage standardizeCells(object, ...)
NULL

#' @export
#' @name tnbc
#' @usage tnbc(object, ...)
NULL
