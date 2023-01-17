## FIXME Need to figure out how to handle seocndary accession remapping.
## e.g. CVCL_2717 to CVCL_1888
## e.g. CVCL_X507 to CVCL_1637



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



## FIXME Add this back in for faster matching...see DepMap Sanger overrides.
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
        cellsOrig <- cells
        cellsStd <- standardizeCells(cells)
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
        ## Note that DepMap currently takes priority over Sanger
        ## CellModelPassports mapping conflicts currently.
        cells <- unlist(Map(
            cell = cells,
            cellStd = standardizeCells(cells),
            f = function(cell, cellStd) {
                switch(
                    EXPR = cellStd,
                    ## DepMap ====
                    "CS1" = "CVCL_T023",
                    "H157" = "CVCL_2458",
                    "HAP1" = "CVCL_Y019",
                    "K2" = "CVCL_AT85",
                    "LY2" = "CVCL_9579",
                    "ML1" = "CVCL_H525",
                    "RC2" = "CVCL_L510",
                    "RH1" = "CVCL_1658",
                    ## Sanger CellModelPassports ====
                    "BT549" = "CVCL_1858",
                    "CJM" = "CVCL_U797",
                    "COLO699" = "CVCL_1992",
                    "COLO699N" = "CVCL_1992",
                    "DL" = "CVCL_U760",
                    "F36E" = "CVCL_2037",
                    "F5" = "CVCL_V616",
                    "JR" = "CVCL_RT33",
                    # > "ML1" = "CVCL_0436", # (conflicts with DepMap).
                    "MM1" = "CVCL_5801",
                    # > "MS1" = "CVCL_E995", # (conflicts with DepMap)
                    "RH3" = "CVCL_L415",
                    "RH4" = "CVCL_5916",
                    "SBC2" = "CVCL_W531",
                    "ST" = "CVCL_U347",
                    cell
                )
            },
            USE.NAMES = FALSE
        ))
        idx <- poolRep[match(x = cells, table = poolUnlist)]
        ## Fall back to matching by standardized cell name.
        ## FIXME Rework this using internal data frame.
        if (anyNA(idx)) {
            naIdx <- which(is.na(idx))
            cells2 <- vapply(
                X = standardizeCells(cells[naIdx]),
                FUN = function(x) {
                    switch(
                        EXPR = x,
                        "CTV1DM" = "CVCL_1150", # DepMap
                        "JURKATCLONEE61" = "CVCL_0367", # CMP
                        "KPNS19S" = "CVCL_1340", # CMP
                        "THUR14TKB" = "CVCL_5953", # CMP
                        "YUHOIN0650" = "CVCL_J521", # DepMap
                        x
                    )
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
