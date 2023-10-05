#' Gene fusion pairs
#'
#' @name geneFusions
#' @note Updated 2023-10-05
#'
#' @return `CharacterList`.
#'
#' @examples
#' data(cello)
#' x <- geneFusions(cello, format = "geneName")
#' print(x)
NULL



## Updated 2023-10-05.
`geneFusions,Cellosaurus` <-
    function(
        object,
        format = c("geneName", "hgncId", "both")) {
        assert(validObject(object))
        format <- match.arg(format)
        x <- mclapply(
            X = object[["comments"]],
            FUN = function(x) {
                x <- x[["Sequence variation"]]
                x <- grep(pattern = "^Gene fusion; ", x = x, value = TRUE)
                x
            }
        )
        names(x) <- rownames(object)
        x <- x[lengths(x) > 0L]
        x <- CharacterList(x)
        x <- sub(
            pattern = paste0(
                "^Gene fusion; ",
                "HGNC; ([0-9]+); ([^ ]+) \\+ ",
                "HGNC; ([0-9]+); ([^ ]+); ",
                "Name\\(s\\)=([^ ,]+).+$"
            ),
            replacement = switch(
                EXPR = format,
                "both" = "\\2-\\4_\\1-\\3",
                "geneName" = "\\5",
                "hgncId" = "\\1-\\3"
            ),
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
