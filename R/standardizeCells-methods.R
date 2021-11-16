#' @name standardizeCells
#' @inherit AcidGenerics::standardizeCells return title
#' @note Updated 2020-10-01.
#'
#' @description
#' Strip all non-alphanumeric characters, remove information in
#' parentheses/brackets, and convert to uppercase.
#'
#' @param object `character`.
#'   Cell line names.
#' @param ... Additional arguments.
#'
#' @examples
#' cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
#' standardizeCells(c("22Rv1", "Jurkat", "Ramos (RA-1)"))
NULL



## Updated 2020-10-01.
`standardizeCells,character` <-  # nolint
    function(object) {
        assert(isCharacter(object))
        object <- tolower(object)
        object <- str_replace(
            string = object,
            pattern = "\\s[\\[\\(].+$",
            replacement = ""
        )
        object <- str_replace_all(
            string = object,
            pattern = "[^[:alnum:]]+",
            replacement = ""
        )
        object <- snakeCase(
            object = object,
            prefix = FALSE,
            smart = FALSE
        )
        object <- toupper(object)
        object
    }



#' @rdname standardizeCells
#' @export
setMethod(
    f = "standardizeCells",
    signature = signature(object = "character"),
    definition = `standardizeCells,character`
)
