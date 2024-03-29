#' @name standardizeCells
#' @inherit AcidGenerics::standardizeCells return title
#' @note Updated 2023-09-21.
#'
#' @details
#' Strip all non-alphanumeric characters, remove information in parentheses or
#' brackets, and convert to uppercase.
#'
#' Note that this function doesn't attempt to perform any mapping to the
#' Cellosaurus database. For that, refer to `mapCells` instead.
#'
#' @param object `character`.
#' Cell line names.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' ## character ====
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



## Updated 2023-09-21.
`standardizeCells,character` <- # nolint
    function(object) {
        assert(
            isCharacter(object),
            hasNoDuplicates(object)
        )
        object <- tolower(object)
        ## Handle "SUM-52PE, SUM52" to "SUM-52PE" edge case.
        object <- gsub(
            pattern = ",\\s.+$",
            replacement = "",
            x = object
        )
        object <- sub(
            pattern = "\\s[\\[\\(].+$",
            replacement = "",
            x = object
        )
        object <- gsub(
            pattern = "[^[:alnum:]]+",
            replacement = "",
            x = object
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
