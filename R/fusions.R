#' Gene fusion pairs
#'
#' @name fusions
#' @note Updated 2023-10-05
#'
#' @return `CharacterList`.
#'
#' @examples
#' data(cello)
#' x <- fusions(cello)
#' print(x)
NULL



## Updated 2023-10-05.
`fusions,Cellosaurus` <-
    function(
        object,
        format = c("hgncId", "geneName", "both")) {
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



#' @rdname fusions
#' @export
setMethod(
    f = "fusions",
    signature = signature(object = "Cellosaurus"),
    definition = `fusions,Cellosaurus`
)
