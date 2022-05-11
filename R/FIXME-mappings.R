## FIXME Still in development.
## FIXME Consider reworking with ontologyX suite (ontologyIndex).

#' Cellosaurus identifier to cell line name mappings
#'
#' @export
#' @note Updated 2022-05-11.
#'
#' @return `DataFrame`.
#' Cell identifier mappings, with `cellId` and `cellName` columns.
#' All rows contain identifiers prefixed with `"CVCL_"`.
mappings <- function() {
    assert(AcidBase::requireNamespaces(c(
        "AcidBase",
        "BiocIO",
        "BiocSet",
        "S4Vectors",
        "pipette"
    )))
    url <- AcidBase::pasteURL(
        "ftp.expasy.org",
        "databases",
        "cellosaurus",
        "cellosaurus.obo",
        protocol = "ftp"
    )
    tmpfile <- pipette::cacheURL(url = url, pkg = .pkgName)
    con <- suppressMessages({
        BiocIO::FileForFormat(tmpfile)
    })
    obo <- import(con = con)
    meta <- S4Vectors::metadata(obo)[["obo_header"]]
    dataVersion <- meta[
        which(meta[["key"]] == "data-version"),
        "value",
        drop = TRUE
    ]
    set <- BiocSet::es_set(obo)
    df <- as(set, "DataFrame")
    keep <- grepl(pattern = "^CVCL_", x = df[["set"]],)
    df <- df[keep, ]
    colnames(df) <- c("cellId", "cellName")
    S4Vectors::metadata(df)[["dataVersion"]] <- version
    df
}
