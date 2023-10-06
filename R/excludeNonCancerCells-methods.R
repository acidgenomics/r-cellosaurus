#' @name excludeNonCancerCells
#' @inherit AcidGenerics::excludeNonCancerCells
#' @note Updated 2023-10-05.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' print(dim(object))
#' object <- excludeNonCancerCells(object)
#' print(dim(object))
NULL



## Updated 2023-10-05.
`excludeNonCancerCells,Cellosaurus` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset("isCancer", colnames(object))
        )
        keep <- object[["isCancer"]]
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname excludeNonCancerCells
#' @export
setMethod(
    f = "excludeNonCancerCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeNonCancerCells,Cellosaurus`
)
