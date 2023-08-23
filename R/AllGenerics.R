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

#' @export
#' @rdname filterProblematicCells
setGeneric(
    name = "filterContaminatedCells",
    def = function(object, ...) {
        standardGeneric("filterContaminatedCells")
    }
)

#' @export
#' @rdname filterProblematicCells
setGeneric(
    name = "filterProblematicCells",
    def = function(object, ...) {
        standardGeneric("filterProblematicCells")
    }
)

#' @export
#' @rdname tnbc
setGeneric(
    name = "tnbc",
    def = function(object, ...) {
        standardGeneric("tnbc")
    }
)
