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
#' x <- mutations(cello, format = "geneName")
#' print(x)
NULL



## Updated 2023-10-05.
`mutations,Cellosaurus` <-
    function(
        object,
        format = c("hgncId", "geneName", "both")) {
        assert(validObject(object))
        format <- match.arg(format)
        x <- mclapply(
            X = object[["comments"]],
            FUN = function(x) {
                x <- x[["Sequence variation"]]
                x <- grep(pattern = "^Mutation; ", x = x, value = TRUE)
                x
            }
        )
        names(x) <- rownames(object)
        x <- x[lengths(x) > 0L]
        x <- CharacterList(x)
        x <- sub(
            pattern = "^Mutation; HGNC; ([0-9]+); ([^;]+);.+$",
            replacement = switch(
                EXPR = format,
                "both" = "\\2_\\1",
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
