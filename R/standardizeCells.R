#' Standardize cell identifiers
#'
#' @export
#' @note Updated 2020-08-02.
#'
#' @details
#' Strip all non-alphanumeric characters, remove information in
#' parentheses/bracket, and convert to uppercase.
#'
#' @param x `character`.
#'   Cell line names.
#'
#' @return `character`.
#'
#' @examples
#' cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
#' standardizeCells(c("22Rv1", "Jurkat", "Ramos (RA-1)"))
standardizeCells <- function(x) {
    assert(isCharacter(x))
    x <- tolower(x)
    x <- str_replace(
        string = x,
        pattern = "\\s[\\[\\(].+$",
        replacement = ""
    )
    x <- str_replace_all(
        string = x,
        pattern = "[^[:alnum:]]+",
        replacement = ""
    )
    x <- snakeCase(
        object = x,
        prefix = FALSE,
        smart = FALSE
    )
    x <- toupper(x)
    x
}
