#' Driver mutations
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @return `CharacterList`.
#'
#' @examples
#' data(cello)
#' x <- driverMutations(cello)
#' print(x)
driverMutations <- function(
        object,
        format = c("geneName", "hgncId", "both")) {
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
