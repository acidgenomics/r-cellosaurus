#' @name standardizeCells
#' @inherit AcidGenerics::standardizeCells return title
#' @note Updated 2023-01-12.
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



## Updated 2022-08-24.
`standardizeCells,Rle` <- # nolint
    function(object) {
        object <- decode(object)
        standardizeCells(object)
    }



## Updated 2023-01-12.
`standardizeCells,character` <- # nolint
    function(object) {
        assert(
            isCharacter(object),
            hasNoDuplicates(object)
        )
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
        if (any(object == "")) {
            object[object == ""] <- "invalid"
        }
        object <- snakeCase(object = object, prefix = FALSE, smart = FALSE)
        object <- toupper(object)
        object
    }



#' @rdname standardizeCells
#' @export
setMethod(
    f = "standardizeCells",
    signature = signature(object = "Rle"),
    definition = `standardizeCells,Rle`
)

#' @rdname standardizeCells
#' @export
setMethod(
    f = "standardizeCells",
    signature = signature(object = "character"),
    definition = `standardizeCells,character`
)
