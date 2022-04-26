#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2022-04-26.
#' @export
#'
#' @return `CellosaurusTable`.
setClass(
    Class = "CellosaurusTable",
    contains = "DFrame"
)
setValidity(
    Class = "CellosaurusTable",
    method = function(object) {
        validate(
            hasRownames(object),
            isSubset(
                x = c("cellLineName", "cellosaurusId"),
                y = colnames(object)
            ),
            allAreMatchingFixed(
                x = object[["cellosaurusId"]],
                pattern = "CVCL_"
            )
        )
    }
)
