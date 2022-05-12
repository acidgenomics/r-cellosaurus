#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2022-05-12.
#' @export
#'
#' @return `Cellosaurus`.
setClass(
    Class = "Cellosaurus",
    contains = "DFrame"
)
setValidity(
    Class = "Cellosaurus",
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
