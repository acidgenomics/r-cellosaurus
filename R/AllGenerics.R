#' @export
#' @name excludeContaminatedCells
#' @usage excludeContaminatedCells(object, ...)
NULL

## FIXME Move to AcidGenerics.

#' @export
#' @rdname excludeNonCancerCells
setGeneric(
    name = "excludeNonCancerCells",
    def = function(object, ...) {
        standardGeneric("excludeNonCancerCells")
    }
)

## FIXME Move to AcidGenerics.

#' @export
#' @rdname excludeNonHumanCells
setGeneric(
    name = "excludeNonHumanCells",
    def = function(object, ...) {
        standardGeneric("excludeNonHumanCells")
    }
)

#' @export
#' @name excludeProblematicCells
#' @usage excludeProblematicCells(object, ...)
NULL

#' @export
#' @name export
#' @usage export(object, con, ...)
NULL

## FIXME Move to AcidGenerics.

#' @export
#' @rdname geneFusions
setGeneric(
    name = "geneFusions",
    def = function(object, ...) {
        standardGeneric("geneFusions")
    }
)

#' @export
#' @name mapCells
#' @usage mapCells(object, ...)
NULL

## FIXME Move to AcidGenerics.

#' @export
#' @rdname mutations
setGeneric(
    name = "mutations",
    def = function(object, ...) {
        standardGeneric("mutations")
    }
)

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
