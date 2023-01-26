#' Triple negative breast cancer (TNBC) cell lines
#'
#' @name tnbc
#' @note Updated 2023-01-26.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#' Cellosaurus accession identifiers.
#'
#' @examples
#' data(cello)
#' tnbc <- tnbc(cello)
NULL



## Updated 2023-01-26.
`tnbc,Cellosaurus` <- function(object) {
    assert(validObject(object))
    lgl <- vapply(
        X = object[["comments"]],
        FUN = function(x) {
            "Triple negative breast cancer (TNBC) cell line" %in% x[["Group"]]
        },
        FUN.VALUE = logical(1L)
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
