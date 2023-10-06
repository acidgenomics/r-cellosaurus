#' @name geneFusions
#' @inherit AcidGenerics::geneFusions
#' @note Updated 2023-10-05.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' x <- geneFusions(object)
#' print(x)
NULL



## Updated 2023-10-05.
`geneFusions,Cellosaurus` <- # nolint
    function(object) {
        assert(validObject(object))
        object <- excludeNonHumanCells(object)
        object <- excludeNonCancerCells(object)
        object <- excludeContaminatedCells(object)
        pattern <- paste0(
            "^Gene fusion; ",
            "HGNC; ([0-9]+); ([^ ]+) \\+ ",
            "HGNC; ([0-9]+); ([^ ]+); ",
            "Name\\(s\\)=([^ ,;]+).+$"
        )
        x <- mclapply(
            X = object[["comments"]],
            FUN = function(x, pattern) {
                x <- x[["Sequence variation"]]
                x <- grep(pattern = pattern, x = x, value = TRUE)
                x
            },
            pattern = pattern
        )
        names(x) <- rownames(object)
        x <- x[lengths(x) > 0L]
        x <- CharacterList(x)
        x <- sub(
            pattern = pattern,
            replacement = "\\5 (\\1-\\3)",
            x = x
        )
        x <- unique(x)
        x
    }



#' @rdname geneFusions
#' @export
setMethod(
    f = "geneFusions",
    signature = signature(object = "Cellosaurus"),
    definition = `geneFusions,Cellosaurus`
)
