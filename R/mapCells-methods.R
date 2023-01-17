#' @name mapCells
#' @inherit AcidGenerics::mapCells description title
#' @note Updated 2023-01-17.
#'
#' @inherit AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param object `character`.
#' Cell names (or Cellosaurus identifiers).
#'
#' @param keyType `character(1)`.
#' Identifier format to return.
#'
#' @return Named `character`.
#' User input in the names and Cellosaurus IDs in the values.
#'
#' @seealso
#' - [Matching values in list](https://stackoverflow.com/questions/11002391/).
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' cells <- head(cello[["name"]])
#' cells <- standardizeCells(cells)
#' cells <- mapCells(object = object, cells = cells)
#' print(cells)
NULL



## Updated 2023-01-17.
`mapCells,Cellosaurus` <- # nolint
    function(object,
             cells,
             keyType = c(
                 "cellosaurusId",
                 "depmapId",
                 "sangerModelId"
             )) {
        assert(
            validObject(object),
            isCharacter(cells),
            hasNoDuplicates(cells)
        )
        keyType <- match.arg(keyType)
        idCol <- switch(
            EXPR = keyType,
            "cellosaurusId" = "id",
            keyType
        )
        ## Early return if all input matches the return key type.
        if (isSubset(x = cells, y = as.character(object[[idCol]]))) {
            return(cells)
        }
        cellsOrig <- cells
        cellsStd <- standardizeCells(cells)
        overrides <- get(
            x = "overrides",
            envir = asNamespace(.pkgName),
            inherits = FALSE
        )
        assert(is.data.frame(overrides))
        ## First create a pool of exact matches annotated in Cellosaurus.
        df <- as(object, "DataFrame")
        cols <- c(
            "id",
            "name",
            "depmapId",
            "sangerModelId",
            "synonym"
        )
        assert(isSubset(cols, colnames(df)))
        df <- df[, cols]
        df[["nameNoBracket"]] <- gsub(
            pattern = "[_ ]+\\[.+$",
            replacement = "",
            x = df[["name"]]
        )
        df[["standardName"]] <- standardizeCells(df[["name"]])
        cols <- append(
            x = cols,
            values = c("nameNoBracket", "standardName")
        )
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
        ## Override any ambiguous cell line names with direct RRID mapping.
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
        idx <- poolRep[match(x = cells, table = poolUnlist)]
        ## Fall back to matching by standardized cell name.
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
            idx2 <- poolRep[match(x = cells2, table = poolUnlist)]
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
