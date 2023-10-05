#' @export
#' @name excludeContaminatedCells
#' @usage excludeContaminatedCells(object, ...)
NULL

#' @export
#' @name excludeProblematicCells
#' @usage excludeProblematicCells(object, ...)
NULL

#' @export
#' @name export
#' @usage export(object, con, ...)
NULL

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
