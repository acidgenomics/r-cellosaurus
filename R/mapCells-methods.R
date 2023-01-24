## FIXME We're now failing to map 13 lines here:
## HA7EBV, NCIBL128, NCIBL1437, Hs-633T, NCIBL1395, NCIBL1770, NCIBL2009,
## NCIBL2052, NCIBL2087, NCIBL209, NCIBL2122, NCIBL2126,
## NCIBL2171



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
        ## First create a pool of exact matches annotated in Cellosaurus.
        df <- as(object, "DataFrame")
        df[["cellLineNameNoBracket"]] <- gsub(
            pattern = "[_ ]+\\[.+$",
            replacement = "",
            x = df[["cellLineName"]]
        )
        .pool <- function(df, cols) {
            assert(isSubset(cols, colnames(df)))
            df <- df[, cols]
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
            poolRep <- rep(
                x = seq_along(pool),
                times = vapply(
                    X = pool,
                    FUN = length,
                    FUN.VALUE = integer(1L)
                )
            )
            poolUnlist <- unlist(pool, recursive = FALSE, use.names = FALSE)
            list(
                "rep" = poolRep,
                "unlist" = poolUnlist
            )
        }
        ## Don't include standardized cell name here. This can result in
        ## unwanted mismatches for some cell lines (e.g. ICC2 [CVCL_VV27] vs.
        ## ICC-2 [CVCL_C6N4]).
        cols <- c(
            "accession",
            "secondaryAccession",
            "depmapId",
            "sangerModelId",
            "cellLineName",
            "cellLineNameNoBracket",
            "synonyms"
        )
        pool <- .pool(df = df, cols = cols)
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
        if (anyNA(idx)) {
            df[["standardName"]] <- standardizeCells(df[["cellLineName"]])
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
            idx2 <- pool[["rep"]][match(x = cells2, table = pool[["unlist"]])]
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
