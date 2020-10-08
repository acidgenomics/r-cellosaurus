#' @name mapCells
#' @inherit AcidGenerics::mapCells description title
#' @note Updated 2020-10-01.
#'
#' @inherit AcidRoxygen::params
#' @param object `character`.
#'   Cell names (or Cellosaurus identifiers).
#' @param ... Additional arguments.
#'
#' @return Named `character`.
#'   User input in the names and Cellosaurus IDs in the values.
#'
#' @examples
#' cells <- c("22RV1", "HS729", "JURKAT")
#' mapCells(cells)
NULL



#' @rdname mapCells
#' @name mapCells
#' @importFrom AcidGenerics mapCells
#' @usage mapCells(object, ...)
#' @export
NULL



## Updated 2020-10-01.
`mapCells,character` <-  # nolint
    function(
        object,
        organism = "Homo sapiens",
        BPPARAM = BiocParallel::bpparam()  # nolint
    ) {
        assert(
            isCharacter(object),
            isString(organism)
        )
        ## Reassigning here, so we can modify with aliases, if necessary.
        cells <- object
        aliases <- import(
            file = system.file(
                "extdata", "aliases.csv",
                package = "cellosaurus"
            ),
            quiet = TRUE
        )
        ## Get the index position of cell names that need to be remapped.
        remap <- which(cells %in% aliases[["input"]])
        if (hasLength(remap)) {
            actual <- match(x = cells[remap], table = aliases[["input"]])
            cells[remap] <- aliases[["actual"]][actual]
        }
        out <- bplapply(
            X = cells,
            FUN = function(x) {
                url <- paste0(
                    "https://web.expasy.org/cgi-bin/cellosaurus/search?input=",
                    URLencode(paste0("\"", x, "\""))
                )
                response <- GET(url)
                content <- content(response, as = "text")
                lines <- strsplit(content, split = "\n")[[1L]]
                lines <- grep(
                    pattern = paste0(">", organism),
                    x = lines,
                    value = TRUE
                )
                lines <- grep(
                    pattern = ">CVCL_",
                    x = lines,
                    value = TRUE
                )
                match <- str_match(
                    string  = lines,
                    pattern = paste0(
                        ">",
                        "(CVCL_[A-Z0-9]+)",
                        "</a></td><td>",
                        "([^<]+)",
                        "</td>"
                    )
                )
                if (!hasRows(match)) return(NA_character_)
                match <- match[, 2L:3L, drop = FALSE]
                match <- unique(match)
                match <- match[order(match[, 1L]), , drop = FALSE]
                which <- match(
                    x = standardizeCells(x),
                    table = standardizeCells(match[, 2L])
                )
                id <- match[which, 1L]
                if (!isString(id)) return(NA_character_)  # nocov
                id
            },
            BPPARAM = BPPARAM
        )
        out <- unlist(out)
        names(out) <- object
        if (any(is.na(out))) {
            fail <- names(out[is.na(out)])
            cli_alert_warning(sprintf(
                fmt = "Failed to match %d %s: %s.",
                length(fail),
                ngettext(n = length(fail), msg1 = "cell", msg2 = "cells"),
                toString(fail, width = 200L)
            ))
        }
        out
    }



#' @rdname mapCells
#' @export
setMethod(
    f = "mapCells",
    signature = signature("character"),
    definition = `mapCells,character`
)
