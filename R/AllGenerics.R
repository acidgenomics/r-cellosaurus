#' @export
#' @name export
#' @usage export(object, con, format, ...)
NULL

#' @export
#' @name mapCells
#' @usage mapCells(object, ...)
NULL

#' @export
#' @name standardizeCells
#' @usage standardizeCells(object, ...)
NULL

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

## FIXME Move to AcidGenerics.

#' @export
#' @rdname tnbc
setGeneric(
    name = "tnbc",
    def = function(object, ...) {
        standardGeneric("tnbc")
    }
)
