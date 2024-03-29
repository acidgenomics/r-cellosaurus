#' @name mapCells
#' @inherit AcidGenerics::mapCells description title
#' @note Updated 2023-09-22.
#'
#' @details
#' This function is designed to take input from a spreadsheet, electronic
#' laboratory notebook entry, or cell line provider, where the cell line names
#' may be inconsistent.
#'
#' If you notice any mapping errors, please file a bug report.
#'
#' @inherit AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param object `character`.
#' Cell names (or Cellosaurus accession identifiers).
#'
#' @param keyType `character(1)`.
#' Identifier format to return.
#'
#' @param strict `logical(1)`.
#' - `FALSE`, return `NA` for cell lines that fail to map.
#' - `TRUE`: Error on mapping failure.
#'
#' @return Named `character`.
#' User input in the names and Cellosaurus accession identifiers in the values.
#'
#' @seealso
#' - [Matching values in list](https://stackoverflow.com/questions/11002391/).
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' cells <- head(cello[["cellLineName"]])
#' print(cells)
#' cells <- mapCells(object = object, cells = cells)
#' print(cells)
NULL



## Updated 2023-09-05.
`mapCells,Cellosaurus` <- # nolint
    function(object,
             cells,
             keyType = c(
                 "cellosaurusId",
                 "depmapId",
                 "sangerModelId",
                 "atccId",
                 "cellLineName"
             ),
             strict = FALSE) {
        if (is(cells, "Rle")) {
            cells <- decode(cells)
        }
        assert(
            validObject(object),
            isCharacter(cells),
            isFlag(strict)
        )
        keyType <- match.arg(keyType)
        idCol <- switch(
            EXPR = keyType,
            "cellosaurusId" = "accession",
            keyType
        )
        ## Early return if all input matches the return key type.
        if (isSubset(x = cells, y = as.character(object[[idCol]]))) {
            return(cells)
        }
        ## Override any ambiguous cell line names with direct Cellosaurus
        ## accession identifier mapping.
        cellsOrig <- cells
        overrides <- get(
            x = "overrides",
            envir = asNamespace(.pkgName),
            inherits = FALSE
        )
        assert(is(overrides, "DFrame"))
        cells <- unlist(Map(
            cell = cells,
            cellStd = standardizeCells(cells),
            MoreArgs = list("overrides" = overrides),
            f = function(cell, cellStd, overrides) {
                idx <- match(x = cellStd, table = overrides[["input"]])
                if (!is.na(idx)) {
                    overrides[idx, "output"]
                } else {
                    cell
                }
            },
            USE.NAMES = FALSE
        ))
        df <- as(object, "DFrame")
        df[["cellLineNameNoBracket"]] <- gsub(
            pattern = "[_ ]+\\[.+$",
            replacement = "",
            x = df[["cellLineName"]]
        )
        cols <- c(
            "accession",
            "secondaryAccession",
            "depmapId",
            "sangerModelId",
            "atccId",
            "cellLineName",
            "cellLineNameNoBracket",
            "synonyms",
            "misspellings"
        )
        table <- df[, intersect(cols, colnames(df))]
        idx <- matchNested(x = cells, table = table)
        ## Fall back to matching against standardized cell name.
        if (anyNA(idx)) {
            naIdx <- which(is.na(idx))
            cells2 <- vapply(
                X = standardizeCells(cells[naIdx]),
                overrides = overrides,
                FUN = function(cell, overrides) {
                    idx <- match(x = cell, table = overrides[["input"]])
                    if (!is.na(idx)) {
                        overrides[idx, "output"]
                    } else {
                        cell
                    }
                },
                FUN.VALUE = character(1L),
                USE.NAMES = FALSE
            )
            table2 <- DataFrame(
                "standardName" = standardizeCells(df[["cellLineName"]]),
                "synonyms" = df[["synonyms"]],
                "misspellings" = df[["misspellings"]]
            )
            idx2 <- matchNested(x = cells2, table = table2)
            idx[naIdx] <- idx2
        }
        cells <- cellsOrig
        if (isTRUE(strict) && anyNA(idx)) {
            fail <- cells[is.na(idx)]
            abort(sprintf(
                fmt = "Failed to map %d %s: %s.",
                length(fail),
                ngettext(n = length(fail), msg1 = "cell", msg2 = "cells"),
                toString(fail, width = 500L)
            ))
        }
        out <- as.character(object[[idCol]])[idx]
        if (isTRUE(strict) && anyNA(out)) {
            fail <- cells[is.na(out)]
            abort(sprintf(
                fmt = "Failed to map %d %s: %s.",
                length(fail),
                ngettext(n = length(fail), msg1 = "cell", msg2 = "cells"),
                toString(fail, width = 500L)
            ))
        }
        names(out) <- cells
        out
    }



#' @rdname mapCells
#' @export
setMethod(
    f = "mapCells",
    signature = signature(object = "Cellosaurus"),
    definition = `mapCells,Cellosaurus`
)
