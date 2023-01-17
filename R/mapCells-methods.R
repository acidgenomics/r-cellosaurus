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



## Dynamically handle common aliases that are difficult to map.
## > aliases <- import(
## >     con = system.file(
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
                x <- toupper(x)
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
        idx <- poolRep[match(x = cells, table = poolUnlist)]
        ## Fall back to matching input based on standardized cell name.
        if (anyNA(idx)) {
            naIdx <- which(is.na(idx))
            idx2 <- poolRep[match(
                x = standardizeCells(cells[naIdx]),
                table = poolUnlist
            )]
            idx[naIdx] <- idx2
        }
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
