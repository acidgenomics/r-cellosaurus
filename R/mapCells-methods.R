#' @name mapCells
#' @inherit AcidGenerics::mapCells description title
#' @note Updated 2022-06-02.
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



## Dynamically handle common aliases that are difficult to map.
## > aliases <- import(
## >     file = system.file(
## >         "extdata", "aliases.csv",
## >         package = .pkgName
## >     ),
## >     quiet = TRUE
## > )
## > remap <- which(cells %in% aliases[["input"]])
## > if (hasLength(remap)) {
## >     actual <- match(x = cells[remap], table = aliases[["input"]])
## >     cells[remap] <- aliases[["actual"]][actual]
## > }



## Updated 2022-06-03.
`mapCells,Cellosaurus` <- # nolint
    function(object,
             cells,
             keyType = c(
                 "cellosaurusId",
                 "depMapId",
                 "sangerId"
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
        if (all(cells %in% object[[idCol]])) {
            return(cells)
        }
        df <- as(object, "DataFrame")
        assert(hasNoDuplicates(df[["name"]]))
        df[["name2"]] <- standardizeCells(df[["name"]])
        assert(hasNoDuplicates(df[["name2"]]))
        matchCols <- c(
            "id",
            "name",
            "name2",
            "depMapId",
            "sangerId",
            "synonym"
        )
        assert(isSubset(matchCols, colnames(df)))
        df <- df[, matchCols]
        pool <- apply(
            X = df,
            MARGIN = 1L,
            FUN = unlist,
            recursive = TRUE,
            use.names = FALSE,
            simplify = FALSE
        )
        g <- rep(
            x = seq_along(pool),
            times = vapply(
                X = pool,
                FUN = length,
                FUN.VALUE = integer(1L)
            )
        )
        idx <- g[match(x = cells, table = unlist(pool))]
        if (anyNA(idx)) {
            fail <- cells[is.na(idx)]
            abort(sprintf(
                fmt = "Failed to map %d %s: %s.",
                length(fail),
                ngettext(n = length(fail), msg1 = "cell", msg2 = "cells"),
                toString(fail, width = 200L)
            ))
        }
        out <- object[[idCol]][idx]
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
