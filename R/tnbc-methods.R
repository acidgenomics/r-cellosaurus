#' Triple negative breast cancer (TNBC) cell lines
#'
#' @name tnbc
#' @note Updated 2023-10-05.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param ... Additional arguments.
#'
#' @return `character`.
#' Cellosaurus accession identifiers.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' tnbc <- tnbc(object)
#' print(tnbc)
NULL



## Updated 2023-10-05.
`tnbc,Cellosaurus` <- # nolint
    function(object) {
        assert(validObject(object))
        object <- excludeNonHumanCells(object)
        object <- excludeNonCancerCells(object)
        object <- excludeProblematicCells(object)
        lgl <- vapply(
            X = object[["comments"]],
            FUN = function(x) {
                paste(
                    "Triple negative breast cancer",
                    "(TNBC) cell line"
                ) %in% x[["Group"]]
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = FALSE
        )
        out <- rownames(object)[lgl]
        out
    }



#' @rdname tnbc
#' @export
setMethod(
    f = "tnbc",
    signature = signature(object = "Cellosaurus"),
    definition = `tnbc,Cellosaurus`
)
