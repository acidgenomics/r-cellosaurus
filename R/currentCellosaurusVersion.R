#' Current Cellosaurus release version
#'
#' @details
#' Queries the server at `ftp.expasy.org`.
#'
#' @export
#' @note Updated 2024-03-11.
#'
#' @return `numeric_version`.
#' Numeric release version.
#'
#' @examples
#' currentCellosaurusVersion()
currentCellosaurusVersion <- function() {
    con <- pasteUrl(
        "ftp.expasy.org",
        "databases",
        "cellosaurus",
        "cellosaurus_relnotes.txt",
        protocol = "https"
    )
    lines <- import(con = con, format = "lines", quiet = TRUE)
    x <- grep(
        pattern = "This is the release notes for Cellosaurus version",
        x = lines,
        value = TRUE
    )
    assert(isString(x))
    x <- strsplit(x, split = " ", fixed = TRUE)[[1L]][[9L]]
    x <- as.integer(x)
    x <- numeric_version(x)
    x
}
