#' @name mapCells
#' @inherit AcidGenerics::mapCells description title
#' @note Updated 2023-01-24.
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
#' @return Named `character`.
#' User input in the names and Cellosaurus accession IDs in the values.
#'
#' @seealso
#' - [Matching values in list](https://stackoverflow.com/questions/11002391/).
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' cells <- standardizeCells(head(cello[["cellLineName"]]))
#' cells <- mapCells(object = object, cells = cells)
#' print(cells)
NULL



## Updated 2023-01-18.
`mapCells,Cellosaurus` <- # nolint
    function(object,
             cells,
             keyType = c(
                 "cellosaurusId",
                 "depmapId",
                 "sangerModelId",
                 "cellLineName"
             )) {
        assert(
            validObject(object),
            isCharacter(cells)
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
        cellsOrig <- cells
        overrides <- get(
            x = "overrides",
            envir = asNamespace(.pkgName),
            inherits = FALSE
        )
        assert(is.data.frame(overrides))
        df <- as(object, "DataFrame")
        df[["cellLineNameNoBracket"]] <- gsub(
            pattern = "[_ ]+\\[.+$",
            replacement = "",
            x = df[["cellLineName"]]
        )
        .pool <- function(df) {
            pool <- apply(
                X = df,
                MARGIN = 1L,
                FUN = function(x) {
                    x <- unlist(x, recursive = TRUE, use.names = FALSE)
                    x <- na.omit(x)
                    x <- unique(x)
                    x
                },
                simplify = FALSE
            )
            rep <- rep(
                x = seq_along(pool),
                times = vapply(
                    X = pool,
                    FUN = length,
                    FUN.VALUE = integer(1L)
                )
            )
            unlist <- unlist(pool, recursive = FALSE, use.names = FALSE)
            list(
                "rep" = rep,
                "unlist" = unlist
            )
        }
        ## First create a pool of exact matches annotated in Cellosaurus.
        pool <- .pool(
            df = df[
                ,
                c(
                    "accession",
                    "secondaryAccession",
                    "depmapId",
                    "sangerModelId",
                    "cellLineName",
                    "cellLineNameNoBracket",
                    "synonyms"
                )
            ]
        )
        ## Override any ambiguous cell line names with direct Cellosaurus
        ## accession identifier mapping.
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
        idx <- pool[["rep"]][match(x = cells, table = pool[["unlist"]])]
        ## Fall back to matching by standardized cell name.
        ## FIXME This isn't working for TM87, but it should...hmm.
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
            df2 <- DataFrame(
                "standardName" = standardizeCells(df[["cellLineName"]]),
                "synonyms" = df[["synonyms"]]
            )
            pool2 <- .pool(df2)
            idx2 <- pool2[["rep"]][match(x = cells2, table = pool2[["unlist"]])]
            idx[naIdx] <- idx2
        }
        cells <- cellsOrig
        if (anyNA(idx)) {
            fail <- cells[is.na(idx)]
            abort(sprintf(
                fmt = "Failed to map %d %s: %s.",
                length(fail),
                ngettext(n = length(fail), msg1 = "cell", msg2 = "cells"),
                toString(fail, width = 500L)
            ))
        }
        out <- as.character(object[[idCol]][idx])
        if (anyNA(out)) {
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
