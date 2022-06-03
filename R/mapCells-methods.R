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
#' @return Named `character`.
#' User input in the names and Cellosaurus IDs in the values.
#'
#' @seealso
#' - [Matching values in list](https://stackoverflow.com/questions/11002391/).
#'
#' @examples
#' ## Cellosaurus ====
#' object <- Cellosaurus()
#' cells <- c("22Rv1", "Jurkat", "Ramos (RA-1)")
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



## Updated 2022-06-02.
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
        matchCols <- c(
            "id",
            "name",
            "depMapId",
            "sangerId",
            "synonym"
        )
        pool <- apply(
            X = as(object, "DataFrame")[, matchCols],
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
