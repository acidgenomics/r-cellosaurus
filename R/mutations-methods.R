#' Driver gene mutations
#'
#' @name mutations
#' @note Updated 2023-10-05.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `CharacterList`.
#'
#' @examples
#' data(cello)
#' x <- mutations(cello)
#' print(x)
NULL



## Updated 2023-10-05.
`mutations,Cellosaurus` <-
    function(
        object,
        format = c("geneName", "hgncId")) {
        assert(validObject(object))
        format <- match.arg(format)
        object <- excludeNonHumanCells(object)
        object <- excludeNonCancerCells(object)
        object <- excludeProblematicCells(object)
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
            replacement = switch(
                EXPR = format,
                "geneName" = "\\2",
                "hgncId" = "\\1"
            ),
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
