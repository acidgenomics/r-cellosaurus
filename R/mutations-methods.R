#' @name mutations
#' @inherit AcidGenerics::mutations
#' @note Updated 2023-10-05.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' x <- mutations(object)
#' print(x)
NULL



## Updated 2023-10-05.
`mutations,Cellosaurus` <- # nolint
    function(object) {
        assert(validObject(object))
        object <- excludeNonHumanCells(object)
        object <- excludeNonCancerCells(object)
        object <- excludeContaminatedCells(object)
        ## Ensure we exclude any human cell lines with weird VGNC mapping here,
        ## such as CVCL_C1GL.
        pattern <- "^Mutation; HGNC; ([0-9]+); ([^;]+);.+$"
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
            replacement = "\\2 (\\1)",
            x = x
        )
        x <- unique(x)
        x
    }



#' @rdname mutations
#' @export
setMethod(
    f = "mutations",
    signature = signature(object = "Cellosaurus"),
    definition = `mutations,Cellosaurus`
)
