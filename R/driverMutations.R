#' Driver mutations
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @examples
#' data(cello)
#' x <- driverMutations(cello)
#' print(x)
driverMutations <- function(object) {
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
    x <- sub(pattern = "Mutation; ", replacement = "", x = x)
    ## Are these organized by a consistent splitting pattern?
    ## That may be easier to sanitize.
    x
}
