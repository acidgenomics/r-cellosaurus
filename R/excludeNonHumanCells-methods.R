#' @name excludeNonHumanCells
#' @inherit AcidGenerics::excludeNonHumanCells
#' @note Updated 2023-10-06.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' print(dim(object))
#' object <- excludeNonHumanCells(object)
#' print(dim(object))
NULL



## Updated 2023-10-05.
`excludeNonHumanCells,Cellosaurus` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset("ncbiTaxonomyId", colnames(object))
        )
        keep <- lengths(object[["ncbiTaxonomyId"]]) == 1L
        object <- object[keep, , drop = FALSE]
        taxIds <- unlist(
            x = object[["ncbiTaxonomyId"]],
            recursive = FALSE,
            use.names = FALSE
        )
        keep <- taxIds == 9606L
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname excludeNonHumanCells
#' @export
setMethod(
    f = "excludeNonHumanCells",
    signature = signature(object = "Cellosaurus"),
    definition = `excludeNonHumanCells,Cellosaurus`
)
