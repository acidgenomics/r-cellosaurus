## FIXME Need to be able to match NCBI taxonomy identifiers using API.
## FIXME Need to be able to match NCI thesaurus identifiers using API.

## Refer to BiocOncoTK for useful functions.
## https://bioconductor.org/packages/devel/bioc/vignettes/BiocOncoTK/inst/doc/maptcga.html
## https://github.com/NCI-Thesaurus/thesaurus-obo-edition/wiki/Downloads



#' Cellosaurus table
#'
#' @name Cellosaurus
#' @note Updated 2022-05-13.
#'
#' @return `Cellosaurus`.
#'
#' @seealso
#' - ftp://ftp.expasy.org/databases/cellosaurus
#' - https://github.com/calipho-sib/cellosaurus
#' - ontologyIndex package
#'
#' @examples
#' ## This is CPU intensive.
#' ## > object <- Cellosaurus()
#' ## > print(object)
NULL



#' Import Cellosaurus data frame from OBO file
#'
#' @note Updated 2022-05-13.
#' @noRd
.importCellosaurus <- function() {
    tmpfile <- cacheURL(
        url = pasteURL(
            "ftp.expasy.org",
            "databases",
            "cellosaurus",
            "cellosaurus.obo",
            protocol = "ftp"
        ),
        pkg = .pkgName
    )
    db <- import(con = tmpfile)
    assert(is(db, "ontology_index"))
    attr <- attr(x = db, which = "version", exact = TRUE)
    dataVersion <- strsplit(
        x = grep(
            pattern = "^data-version:",
            x = attr,
            value = TRUE
        ),
        split = ": ",
        fixed = TRUE
    )[[1L]][[2L]]
    dataVersion <- as.numeric_version(dataVersion)
    ## S3 coercion method here is defined in ontologyIndex package.
    df <- as.data.frame(db)
    df <- as(df, "DataFrame")
    df <- sanitizeNA(df)
    df <- removeNA(df)
    colnames(df) <- camelCase(colnames(df))
    keep <- grepl(pattern = "^CVCL_", x = df[["id"]])
    df <- df[keep, ]
    df <- df[order(rownames(df)), ]
    metadata(df)[["dataVersion"]] <- dataVersion
    df
}



#' Split comments by sentence
#'
#' @note Updated 2022-05-13.
#' @noRd
.splitComments <- function(object) {
    assert(is(object, "DataFrame"))
    object[["comment"]] <- gsub(
        pattern = "^\"",
        replacement = "",
        x = object[["comment"]]
    )
    object[["comment"]] <- gsub(
        pattern = "\"$",
        replacement = "",
        x = object[["comment"]]
    )
    object <- .splitCol(
        object = object,
        colName = "comment",
        split = ". "
    )
    object[["comment"]] <- gsub(
        pattern = "\\.$",
        replacement = "",
        x = object[["comment"]]
    )
    object
}



#' Split synonyms column
#'
#' @note Updated 2022-05-13.
#' @noRd
.splitSynonyms <- function(object) {
    assert(is(object, "DataFrame"))
    object[["synonym"]] <-
        gsub(
            pattern = " RELATED []",
            replacement = "",
            x = object[["synonym"]],
            fixed = TRUE,
        )
    object[["synonym"]] <-
        gsub(
            pattern = "\"",
            replacement = "",
            x = object[["synonym"]],
            fixed = TRUE,
        )
    object <- .splitCol(object, colName = "synonym")
    object
}



#' Split a column into a character list
#'
#' @note Updated 2022-05-13.
#' @noRd
.splitCol <- function(object, colName, split = "; ") {
    assert(
        is(object, "DataFrame"),
        isString(colName),
        isString(split)
    )
    object[[colName]] <-
        CharacterList(strsplit(
            x = object[[colName]],
            split = split,
            fixed = TRUE
        ))
    object
}



## FIXME Need to consolidate this code with Sanger extractor...

#' Extract DepMap identifiers
#'
#' @note Updated 2022-05-13.
#' @noRd
.extractDepMapIds <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["xref"]], "CharacterList")
    )
    depMapId <- grep(
        pattern = "^DepMap:",
        x = object[["xref"]],
        value = TRUE
    )
    depMapId <- gsub(
        pattern = "^DepMap:",
        replacement = "",
        x = depMapId
    )
    depMapId <- vapply(
        X = depMapId,
        FUN = function(x) {
            if (identical(x, character())) {
                return(NA_character_)
            }
            x[[1L]]
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    object[["depMapId"]] <- depMapId
    object
}



#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function() {
        object <- .importCellosaurus()
        object <- .splitComments(object)
        object <- .splitSynonyms(object)
        object <- .splitCol(object, colName = "originateFromSameIndividualAs")
        object <- .splitCol(object, colName = "subset")
        object <- .splitCol(object, colName = "xref")
        object <- .extractDepMapIds(object)
        object[["isCancer"]] <-
            any(object[["subset"]] == "Cancer_cell_line")
        object[["isProblematic"]] <-
            any(grepl(
                pattern = "Problematic cell line",
                x = object[["comment"]],
                fixed = TRUE
            ))
        ## Extract Sanger Cell Model Passports identifiers.
        sangerId <- grep(
            pattern = "^Cell_Model_Passport:",
            x = df[["xref"]],
            value = TRUE
        )
        sangerId <- gsub(
            pattern = "^Cell_Model_Passport:",
            replacement = "",
            x = cmpId
        )
        sangerId <- vapply(
            X = cmpId,
            FUN = function(x) {
                if (identical(x, character())) {
                    return(NA_character_)
                }
                x[[1L]]
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
        df[["sangerId"]] <- sangerId
        ## Indicate which cell lines are cancer.

        ## Extract taxonomy identifiers, for organism matching.
        ## e.g. "NCBI_TaxID:10090"
        taxId <- grep(
            pattern = "^NCBI_TaxID:",
            x = df[["xref"]],
            value = TRUE
        )
        taxId <- gsub(
            pattern = "^NCBI_TaxID:",
            replacement = "",
            x = taxId
        )
        taxId <- vapply(
            X = taxId,
            FUN = function(x) {
                if (identical(x, character())) {
                    return(NA_character_)
                }
                x[[1L]]
            },
            FUN.VALUE = character(1L)
        )
        taxId <- as.factor(taxId)
        df[["taxId"]] <- taxId

        ## FIXME How to get a complete list of these?
        ## https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi
        ## Genbank common name is also included here in parentheses.
        df[["organism"]] <- as.factor(vapply(
            X = df[["taxId"]],
            FUN = function(x) {
                switch(
                    EXPR = x,
                    "7227" = "Drosophila melanogaster (fruit fly)",
                    "9031" = "Gallus gallus (chicken)",
                    "9483" = "Callithrix jacchus (white-tufted-ear marmoset)",
                    "9534" = "Chlorocebus aethiops (grivet)",
                    "9544" = "Macaca mulatta (Rhesus monkey)",
                    "9598" = "Pan troglodytes (chimpanzee)",
                    "9606" = "Homo sapiens (human)",
                    "9615" = "Canis lupus familiaris (dog)",
                    "9796" = "Equus caballus (horse)",
                    "9823" = "Sus scrofa (pig)",
                    "9913" = "Bos taurus (cattle)",
                    "9925" = "Capra hircus (goat)",
                    "9940" = "Ovis aries (sheep)",
                    "9986" = "Oryctolagus cuniculus (rabbit)",
                    "10029" = "Cricetulus griseus (Chinese hamster)",
                    "10036" = "Mesocricetus auratus (golden hamster)",
                    "10090" = "Mus musculus (house mouse)",
                    "10116" = "Rattus norvegicus (Norway rat)",
                    NA_character_
                )
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ))

        ## FIXME NCI thesaurus OBO.
        ## > obo <- import("http://purl.obolibrary.org/obo/ncit.obo")
        ## > df <- as.data.frame(obo)
        ## > df <- as(df, "DataFrame")
        ## > df <- df[, c("id", "name")]
        ## FIXME How to subset the identifiers we want here specifically?

        metadata(df)[["dataVersion"]] <- dataVersion
        df <- df[, sort(colnames(df)), drop = FALSE]
        new("CellosaurusTable", df)
    }
