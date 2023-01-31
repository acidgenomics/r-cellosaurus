#' Triple negative breast cancer (TNBC) cell lines
#'
#' @name tnbc
#' @note Updated 2023-01-31.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#' Cellosaurus accession identifiers.
#'
#' @examples
#' data(cello)
#' tnbc <- tnbc(cello)
#' print(tnbc)
NULL



## Updated 2023-01-31.
`tnbc,Cellosaurus` <- # nolint
    function(object) {
        assert(validObject(object))
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
