index <- function() {
    assert(AcidBase::requireNamespaces(c(
        "AcidBase",
        "ontologyIndex",
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
    ## Alternatively can use `get_OBO`.

    ## NOTE This step is a bit slow.
    xxx <- ontologyIndex::get_relation_names(file = tmpfile)
    ## > [1] "derived_from"
    ## > [2] "originate_from_same_individual_as"

    ## FIXME This is what we want to include in pipette for OBOFile.

    obo <- ontologyIndex::get_ontology(
        file = tmpfile,
        propagate_relationships = "is_a",
        extract_tags = "everything" # or "minimal"
    )
    df <- as.data.frame(obo)
    df <- as(df, "DataFrame")
    ## FIXME Need to import these.
    df <- sanitizeNA(df)
    df <- removeNA(df)
    keep <- grepl(pattern = "^CVCL_", x = df[["id"]])
    df <- df[keep, ]
    df <- df[order(rownames(df)), ]

    ## FIXME Format the column names in camel case.

    ## Sanitize quotes in comments.
    df[["comment"]] <- gsub(
        pattern = "^\"",
        replacement = "",
        x = df[["comment"]]
    )
    df[["comment"]] <- gsub(
        pattern = "\"$",
        replacement = "",
        x = df[["comment"]]
    )

    ## FIXME Split comment by " ;".
    ##   Need to remove leading trailing '"'.
    ##
    ## FIXME Split the xref column into identifiers.
    ## FIXME Split "subset" column by " ;".
    ## FIXME For synonym, need to sanitize.

## FIXME Add this to metadata.
    meta <- attr(x = obo, which = "version", exact = TRUE)
    dataVersion <- strsplit(
        x = grep(
            pattern = "^data-version:",
            x = meta,
            value = TRUE
        ),
        split = ": ",
        fixed = TRUE
    )[[1L]][[2L]]


}
