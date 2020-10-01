#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2020-10-01.
#' @export
#'
#' @return `CellosaurusTable`.
setClass(
    Class = "CellosaurusTable",
    contains = "DataFrame"
)
setValidity(
    Class = "CellosaurusTable",
    method = function(object) {
        validate(
            hasRownames(object),
            isSubset(
                x = c("cellLineName", "cellosaurusID"),
                y = colnames(object)
            ),
            allAreMatchingFixed(
                x = object[["cellosaurusID"]],
                pattern = "CVCL_"
            )
        )
    }
)
