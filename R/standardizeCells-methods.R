#' @name standardizeCells
#' @inherit AcidGenerics::standardizeCells return title
#' @note Updated 2022-06-03.
#'
#' @details
#' Strip all non-alphanumeric characters, remove information in
#' parentheses/brackets, and convert to uppercase.
#'
#' @param object `character`.
#' Cell line names.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' object <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
#' object <- standardizeCells(object)
#' print(object)
NULL



## Updated 2022-06-03.
`standardizeCells,character` <- # nolint
    function(object) {
        assert(isCharacter(object))
        object <- tolower(object)
        object <- stri_replace_first_regex(
            str = object,
            pattern = "\\s[\\[\\(].+$",
            replacement = ""
        )
        object <- stri_replace_all_regex(
            str = object,
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
