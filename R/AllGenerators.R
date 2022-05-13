## FIXME Need to be able to match NCBI taxonomy identifiers using API.



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
#' object <- Cellosaurus()
#' print(object)
NULL



#' Extract DepMap identifiers
#'
#' @note Updated 2022-05-13.
#' @noRd
.addDepMapIds <- function(object) {
    .extractXref(
        object = object,
        colName = "depMapId",
        keyName = "DepMap"
    )
}



#' Add `isCancer` column
#'
#' @note Updated 2022-05-13.
#' @noRd
.addIsCancer <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["subset"]], "CharacterList")
    )
    object[["isCancer"]] <-
        any(object[["subset"]] == "Cancer_cell_line")
    object
}



#' Add `isProblematic` column
#'
#' @note Updated 2022-05-13.
#' @noRd
.addIsProblematic <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comment"]], "CharacterList")
    )
    object[["isProblematic"]] <-
        any(grepl(
            pattern = "Problematic cell line",
            x = object[["comment"]],
            fixed = TRUE
        ))
    object
}



#' Extract NCBI taxonomy identifiers
#'
#' @note Updated 2022-05-13.
#' @noRd
.addNcbiTaxonomyIds <- function(object) {
    colName <- "ncbiTaxonomyId"
    object <- .extractXref(
        object = object,
        colName = colName,
        keyName = "NCBI_TaxID"
    )
    object[[colName]] <- as.integer(object[[colName]])
    object[[colName]] <- Rle(object[[colName]])
    object
}



#' Extract NCI thesaurus disease identifiers
#'
#' @note Updated 2022-05-13.
#' @noRd
.addNcitDiseaseId <- function(object) {
    colName <- "ncitDiseaseId"
    object <- .extractXref(
        object = object,
        colName = colName,
        keyName = "NCIt"
    )
    object[[colName]] <- Rle(object[[colName]])
    object
}



#' Add NCI thesaurus disease names, using OBO ontology file
#'
#' @note Updated 2022-05-13.
#' @noRd
#'
#' @seealso
#' - BiocOncoTK package
#' - https://github.com/NCI-Thesaurus/thesaurus-obo-edition/wiki/Downloads
.addNcitDiseaseName <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["ncitDiseaseId"]], "Rle")
    )
    tmpfile <- cacheURL(
        url = pasteURL(
            "purl.obolibrary.org",
            "obo",
            "ncit.obo",
            protocol = "http"
        ),
        pkg = .pkgName
    )
    ## NOTE May be able to speed this up by setting "minimal" instead of
    ## "everything" in ontologyIndex call, which would need to be added to
    ## pipette in a future update.
    db <- import(con = tmpfile)
    names <- db[["name"]]
    map <- DataFrame(
        "ncitDiseaseId" = names(names),
        "ncitDiseaseName" = unname(names)
    )
    map[["ncitDiseaseId"]] <- gsub(
        pattern = "^NCIT:",
        replacement = "",
        x = map[["ncitDiseaseId"]]
    )
    out <- leftJoin(x = object, y = map, by = "ncitDiseaseId")
    out[["ncitDiseaseId"]] <- Rle(out[["ncitDiseaseId"]])
    out
}



## FIXME Rework this to look up taxonomy IDs from NCBI instead of manual input.

#' Add `organism` column, using NCBI taxonomy
#'
#' Genbank common name is also included here in parentheses.
#'
#' @note Updated 2022-05-13.
#' @noRd
#'
#' @seealso
#' - https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi
.addOrganism <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["ncbiTaxonomyId"]], "Rle")
    )
    value <- vapply(
        X = as.character(object[["ncbiTaxonomyId"]]),
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
    )
    value <- Rle(value)
    object[["organism"]] <- value
    object
}



#' Extract Sanger Cell Model Passports identifiers
#'
#' @note Updated 2022-05-13.
#' @noRd
.addSangerIds <- function(object) {
    .extractXref(
        object = object,
        colName = "sangerId",
        keyName = "Cell_Model_Passport"
    )
}



#' Extract a key-value pair from xref metadata
#'
#' @note Updated 2022-05-13.
#' @noRd
.extractXref <- function(object, colName, keyName) {
    assert(
        is(object, "DataFrame"),
        is(object[["xref"]], "CharacterList"),
        isString(colName),
        isString(keyName)
    )
    pattern <- paste0("^(", keyName, "):([[:space:]])?")
    x <- grep(
        pattern = pattern,
        x = object[["xref"]],
        value = TRUE
    )
    x <- gsub(
        pattern = pattern,
        replacement = "",
        x = x
    )
    x <- vapply(
        X = x,
        FUN = function(x) {
            if (identical(x, character())) {
                return(NA_character_)
            }
            x[[1L]]
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    object[[colName]] <- x
    object
}



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
        object <- .addDepMapIds(object)
        object <- .addSangerIds(object)
        object <- .addNcbiTaxonomyIds(object)
        object <- .addIsCancer(object)
        object <- .addIsProblematic(object)
        object <- .addOrganism(object)
        object <- .addNcitDiseaseId(object)
        object <- .addNcitDiseaseName(object)
        object <- object[, sort(colnames(object)), drop = FALSE]
        new("Cellosaurus", object)
    }
