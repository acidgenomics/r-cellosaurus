#' Cellosaurus table
#'
#' @name Cellosaurus
#' @note Updated 2023-01-12.
#'
#' @details
#' Patient age is currently only defined in `cellosaurus.txt` file but
#' not `cellosaurus.obo` available from the FTP server.
#'
#' @param cache `logical(1)`.
#' Cache files using `BiocFileCache`.
#'
#' @return `Cellosaurus`.
#'
#' @seealso
#' - https://www.cellosaurus.org/
#' - ftp://ftp.expasy.org/databases/cellosaurus/
#' - https://github.com/calipho-sib/cellosaurus/
#'
#' @examples
#' object <- Cellosaurus()
#' print(object)
NULL



#' Extract DepMap identifiers
#'
#' @note Updated 2022-10-04.
#' @noRd
.addDepMapIds <- function(object) {
    .extractXref(
        object = object,
        colName = "depmapId",
        keyName = "DepMap"
    )
}



#' Add `ethnicity` column
#'
#' @note Updated 2022-09-13.
#' @noRd
.addEthnicity <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comment"]], "CharacterList")
    )
    x <- object[["comment"]]
    x <- grep(pattern = "^Population:", x = x, value = TRUE)
    x <- gsub(pattern = "^Population: ", replacement = "", x = x)
    x <- CharacterList(lapply(
        X = x,
        FUN = function(x) {
            if (identical(x, character())) {
                return(character())
            }
            strsplit(x = x, split = "; ")[[1L]]
        }
    ))
    object[["ethnicity"]] <- x
    object
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



#' Add `msiStatus` column
#'
#' @note Updated 2022-09-13.
#' @noRd
.addMsiStatus <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comment"]], "CharacterList")
    )
    x <- object[["comment"]]
    x <- grep(
        pattern = "^Microsatellite instability: ",
        x = x,
        value = TRUE
    )
    x <- gsub(
        pattern = "^Microsatellite instability: ",
        replacement = "",
        x = x
    )
    x <- as.character(x)
    assert(identical(length(x), nrow(object)))
    object[["msiStatus"]] <- x
    object
}



#' Extract NCBI taxonomy identifiers
#'
#' @note Updated 2022-08-23.
#' @noRd
.addNcbiTaxonomyIds <- function(object) {
    colName <- "ncbiTaxonomyId"
    object <- .extractXref(
        object = object,
        colName = colName,
        keyName = "NCBI_TaxID"
    )
    object[[colName]] <- as.integer(object[[colName]])
    object
}



#' Extract NCI thesaurus disease identifiers
#'
#' @note Updated 2022-08-23.
#' @noRd
.addNcitDiseaseId <- function(object) {
    .extractXref(
        object = object,
        colName = "ncitDiseaseId",
        keyName = "NCIt"
    )
}



#' Add NCI thesaurus disease names, using OBO ontology file
#'
#' @note Updated 2022-09-13.
#' @noRd
#'
#' @details
#' Alternative versioned release:
#' https://github.com/NCI-Thesaurus/thesaurus-obo-edition/releases/
#' download/v2022-08-19/ncit.obo
#'
#' May be able to speed this up by setting "minimal" instead of "everything" in
#' ontologyIndex call, which would need to be added to pipette in the future.
#'
#' @seealso
#' -https://evs.nci.nih.gov/evs-download/thesaurus-downloads
#' - https://obofoundry.org/ontology/ncit.html
#' - https://github.com/NCI-Thesaurus/thesaurus-obo-edition/
#' - https://www.ebi.ac.uk/ols/ontologies/ncit
#' - BiocOncoTK package
.addNcitDiseaseName <- function(object, cache) {
    assert(isFlag(cache))
    url <- pasteURL(
        "purl.obolibrary.org",
        "obo",
        "ncit.obo",
        protocol = "http"
    )
    if (isTRUE(cache)) {
        con <- cacheURL(url = url, pkg = .pkgName)
    } else {
        con <- url
    }
    db <- import(con)
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
    out
}



#' Add `organism` column, using NCBI taxonomy identifiers as input
#'
#' @note Updated 2022-08-23.
#' @noRd
#'
#' @seealso
#' - https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi
#' - https://ftp.ncbi.nih.gov/pub/taxonomy/taxdump_readme.txt
.addOrganism <- function(object) {
    alert("Mapping NCBI taxonomy identifiers to organism name.")
    db_download_ncbi(verbose = FALSE, overwrite = FALSE)
    suppressWarnings({
        org <- taxid2name(
            x = as.character(object[["ncbiTaxonomyId"]]),
            db = "ncbi"
        )
    })
    object[["organism"]] <- org
    object
}



#' Add `samplingSite` column
#'
#' @note Updated 2022-09-13.
#' @noRd
.addSamplingSite <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comment"]], "CharacterList")
    )
    x <- object[["comment"]]
    x <- grep(
        pattern = "^Derived from sampling site: ",
        x = x,
        value = TRUE
    )
    x <- gsub(
        pattern = "^Derived from sampling site: ",
        replacement = "",
        x = x
    )
    x <- as.character(x)
    assert(identical(length(x), nrow(object)))
    object[["samplingSite"]] <- x
    object
}



#' Extract Sanger Cell Model Passports identifiers
#'
#' @note Updated 2022-10-04.
#' @noRd
.addSangerIds <- function(object) {
    .extractXref(
        object = object,
        colName = "sangerModelId",
        keyName = "Cell_Model_Passport"
    )
}



## Updated 2022-08-23.
.addSex <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["subset"]], "CharacterList")
    )
    sex <- rep(NA_character_, times = nrow(object))
    femaleIdx <- which(any(object[["subset"]] == "Female"))
    maleIdx <- which(any(object[["subset"]] == "Male"))
    sex[femaleIdx] <- "Female"
    sex[maleIdx] <- "Male"
    object[["sex"]] <- sex
    object
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
#' @note Updated 2023-01-12.
#' @noRd
#'
#' @seealso
#' - https://github.com/calipho-sib/cellosaurus/
#' - https://ftp.expasy.org/databases/cellosaurus/
.importCellosaurus <- function(cache) {
    assert(isFlag(cache))
    url <- pasteURL(
        "r.acidgenomics.com",
        "extdata",
        "cellosaurus",
        paste0("cellosaurus-", .release, ".obo"),
        protocol = "https"
    )
    if (isTRUE(cache)) {
        con <- cacheURL(url, pkg = .pkgName)
    } else {
        con <- url
    }
    db <- import(con)
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
    df[["ancestors"]] <- NULL
    df <- as(df, "DataFrame")
    colnames(df) <- camelCase(colnames(df))
    keep <- grepl(pattern = "^CVCL_", x = df[["id"]])
    df <- df[keep, ]
    df <- df[order(rownames(df)), ]
    ## These steps need to come after selection of rows with valid identifiers.
    df <- sanitizeNA(df)
    df <- removeNA(df)
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
            fixed = TRUE
        )
    object[["synonym"]] <-
        gsub(
            pattern = "\"",
            replacement = "",
            x = object[["synonym"]],
            fixed = TRUE
        )
    object <- .splitCol(object, colName = "synonym")
    object
}



#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function(cache = TRUE) {
        assert(isFlag(cache))
        object <- .importCellosaurus(cache = cache)
        object <- .splitComments(object)
        object <- .splitSynonyms(object)
        object <- .splitCol(object, colName = "originateFromSameIndividualAs")
        object <- .splitCol(object, colName = "subset")
        object <- .splitCol(object, colName = "xref")
        object <- .addNcbiTaxonomyIds(object)
        object <- .addNcitDiseaseId(object)
        object <- .addNcitDiseaseName(object, cache = cache)
        object <- .addOrganism(object)
        object <- .addSex(object)
        object <- .addDepMapIds(object)
        object <- .addSangerIds(object)
        object <- .addIsCancer(object)
        object <- .addIsProblematic(object)
        object <- .addEthnicity(object)
        object <- .addMsiStatus(object)
        object <- .addSamplingSite(object)
        object <- factorize(object)
        object <- encode(object)
        object <- object[, sort(colnames(object)), drop = FALSE]
        new("Cellosaurus", object)
    }
