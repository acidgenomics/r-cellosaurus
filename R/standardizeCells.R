#' Standardize cell identifiers
#'
#' @export
#' @note Updated 2020-08-02.
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
    x
}
