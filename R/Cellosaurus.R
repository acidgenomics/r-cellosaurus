#' Cellosaurus table
#'
#' @name Cellosaurus
#' @note Updated 2023-01-24.
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



#' Add `depmapId` column
#'
#' @details
#' Note that some cell lines, such as "CVCL_0041", map to multiple DepMap
#' identifiers, which is confusing.
#'
#' @note Updated 2023-01-24.
#' @noRd
.addDepmapId <- function(object) {
    .extractCrossRef(object = object, colName = "depmapId", keyName = "DepMap")
}



#' Add `isCancer` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addIsCancer <- function(object) {
    assert(
        is(object, "DataFrame"),
        isCharacter(object[["category"]])
    )
    lgl <- object[["category"]] == "Cancer cell line"
    object[["isCancer"]] <- lgl
    object
}



#' Add `isProblematic` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addIsProblematic <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "SimpleList")
    )
    lgl <- vapply(
        X = object[["comments"]],
        FUN = function(x) {
            "Problematic cell line" %in% names(x)
        },
        FUN.VALUE = logical(1L)
    )
    object[["isProblematic"]] <- lgl
    object
}



#' Add `msiStatus` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addMsiStatus <- function(object) {
    .extractComment(
        object = object,
        colName = "msiStatus",
        keyName = "Microsatellite instability"
    )
}



#' Extract NCI thesaurus disease identifiers
#'
#' @details
#' Some cell lines rarely map to multiple identifiers, such as "CVCL_0028".
#'
#' @note Updated 2023-01-24.
#' @noRd
.addNcitDisease <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["diseases"]], "SimpleList")
    )
    spl <- lapply(
        X = object[["diseases"]],
        FUN = function(x) {
            x <- x[["NCIt"]]
            if (is.null(x)) {
                return(NULL)
            }
            x <- stri_split_fixed(
                str = x,
                pattern = "; ",
                n = 2L,
                simplify = TRUE
            )
            x
        }
    )
    id <- CharacterList(lapply(X = spl, FUN = `[`, j = 1L))
    name <- CharacterList(lapply(X = spl, FUN = `[`, j = 2L))
    object[["ncitDiseaseId"]] <- id
    object[["ncitDiseaseName"]] <- name
    object
}



#' Add `population` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addPopulation <- function(object) {
    .extractComment(
        object = object,
        colName = "population",
        keyName = "Population"
    )
}



#' Add `samplingSite` column
#'
#' @details
#' Note that some comments have additional information on the cell type, that
#' we don't want to include here (e.g. CVCL_0003, CVCL_0008).
#'
#' @note Updated 2023-01-24.
#' @noRd
.addSamplingSite <- function(object) {
    .extractComment(
        object = object,
        colName = "samplingSite",
        keyName = "Derived from sampling site"
    )
}



#' Add `sangerModelId` column
#'
#' @note Updated 2023-01-23.
#' @noRd
.addSangerModelId <- function(object) {
    .extractCrossRef(
        object = object,
        colName = "sangerModelId",
        keyName = "Cell_Model_Passport"
    )
}



#' Add `ncbiTaxonomyId` and `organism` columns
#'
#' @note Updated 2023-01-24.
#' @noRd
.addTaxonomy <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["speciesOfOrigin"]], "CharacterList")
    )
    spl <- lapply(
        X = object[["speciesOfOrigin"]],
        FUN = stri_split_fixed,
        pattern = "; ! ",
        n = 2L,
        simplify = TRUE
    )
    taxId <- IntegerList(lapply(
        X = spl,
        FUN = function(x) {
            sub(
                pattern = "NCBI_TaxID=",
                replacement = "",
                x = x[, 1L]
            )
        }
    ))
    organism <- CharacterList(lapply(
        X = spl,
        FUN = function(x) {
            x[, 2L]
        }
    ))
    object[["ncbiTaxonomyId"]] <- taxId
    object[["organism"]] <- organism
    object[["speciesOfOrigin"]] <- NULL
    object
}



#' Extract and assign identifier column from `crossReferences`
#'
#' @details
#' Note that this intentionally picks only the first matching identifier.
#'
#' @note Updated 2023-01-24.
#' @noRd
.extractCrossRef <- function(object, colName, keyName) {
    assert(
        is(object, "DataFrame"),
        is(object[["crossReferences"]], "SimpleList"),
        isString(colName),
        isString(keyName)
    )
    x <- vapply(
        X = object[["crossReferences"]],
        keyName = keyName,
        FUN = function(x, keyName) {
            ids <- x[[keyName]]
            if (is.null(ids)) {
                return(NA_character_)
            }
            ## Return only the first match.
            ids[[1L]]
        },
        FUN.VALUE = character(1L)
    )
    object[[colName]] <- x
    object
}



#' Extract a key value pair from comments
#'
#' @note Updated 2023-01-24.
#' @noRd
.extractComment <- function(object, colName, keyName) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "SimpleList"),
        isString(colName),
        isString(keyName)
    )
    x <- object[["comments"]]
    x <- CharacterList(lapply(
        X = x,
        FUN = function(x) {
            if (is.null(x[[keyName]])) {
                return(NULL)
            }
            x[[keyName]][[1L]]
        }
    ))
    object[[colName]] <- x
    object
}



## ---------  ---------------------------     ----------------------
## Line code  Content                         Occurrence in an entry
## ---------  ---------------------------     ----------------------
## ID         Identifier (cell line name)     Once; starts an entry
## AC         Accession (CVCL_xxxx)           Once
## AS         Secondary accession number(s)   Optional; once
## SY         Synonyms                        Optional; once
## DR         Cross-references                Optional; once or more
## RX         References identifiers          Optional: once or more
## WW         Web pages                       Optional; once or more
## CC         Comments                        Optional; once or more
## ST         STR profile data                Optional; twice or more
## DI         Diseases                        Optional; once or more
## OX         Species of origin               Once or more
## HI         Hierarchy                       Optional; once or more
## OI         Originate from same individual  Optional; once or more
## SX         Sex of cell                     Optional; once
## AG         Age of donor at sampling        Optional; once
## CA         Category                        Once
## DT         Date (entry history)            Once
## //         Terminator                      Once; ends an entry



#' Import Cellosaurus data frame from TXT file
#'
#' @note Updated 2023-01-24.
#' @noRd
#'
#' @seealso
#' - https://github.com/calipho-sib/cellosaurus/
#' - https://ftp.expasy.org/databases/cellosaurus/
.importCelloFromTxt <- function() {
    url <- pasteURL(
        "ftp.expasy.org",
        "databases",
        "cellosaurus",
        "cellosaurus.txt",
        protocol = "https"
    )
    con <- cacheURL(url, pkg = .pkgName)
    lines <- import(con, format = "lines", engine = "data.table")
    ## Extract the Cellosaurus data version (release) from the top of the file.
    dataVersion <- strsplit(
        x = grep(
            pattern = "^ Version:",
            x = lines[1L:10L],
            value = TRUE
        ),
        split = ": ",
        fixed = TRUE
    )[[1L]][[2L]]
    dataVersion <- as.numeric_version(dataVersion)
    alert(sprintf(
        "Detected Cellosaurus release %s.",
        as.character(dataVersion)
    ))
    alert(sprintf("Mapping lines into {.cls %s} of entries.", "list"))
    x <- Map(
        f = function(lines, i, j) {
            lines[i:(j-1L)]
        },
        i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
        j = grep(pattern = "^//$", x = lines, value = FALSE),
        MoreArgs = list("lines" = lines),
        USE.NAMES = FALSE
    )
    nestedKeys <- c("CC", "DI", "DR", "HI", "OI", "OX", "RX", "ST", "WW")
    optionalKeys <- c("AG", "AS", "SX", "SY")
    .processEntry <- function(x, nestedKeys, optionalKeys) {
        x <- stri_split_fixed(
            str = x,
            pattern = "   ",
            n = 2L,
            simplify = TRUE
        )
        x <- split(x[, 2L], f = x[, 1L])
        for (key in optionalKeys) {
            if (is.null(x[[key]])) {
                x[[key]] <- NA_character_
            }
        }
        for (key in nestedKeys) {
            x[[key]] <- list(unique(x[[key]]))
        }
        x
    }
    if (isTRUE(requireNamespace("future.apply", quietly = TRUE))) {
        alert(sprintf(
            "Processing entries with {.pkg %s}::{.fun %s}.",
            "future.apply", "future_lapply"
        ))
        .lapply <- future.apply::future_lapply
    } else {
        alert("Processing entries.")
        alertInfo(sprintf(
            "Install {.pkg %s} package to speed up this step.",
            "future.apply"
        ))
    }
    x <- .lapply(
        X = x,
        FUN = .processEntry,
        nestedKeys = nestedKeys,
        optionalKeys = optionalKeys
    )
    alert(sprintf(
        "Coercing entries list to {.cls %s} with {.pkg %s}::{.fun %s}.",
        "data.frame", "data.table", "rbindlist"
    ))
    ## Alternatively, can use `AcidPlyr::mapToDataFrame` here, but is slower.
    df <- rbindlist(l = x, use.names = TRUE, fill = FALSE)
    df <- as(df, "DataFrame")
    for (key in nestedKeys) {
        assert(is.list(df[[key]]))
        df[[key]] <- CharacterList(df[[key]])
    }
    colnames(df)[colnames(df) == "AC"] <- "accession"
    colnames(df)[colnames(df) == "AG"] <- "ageAtSampling"
    colnames(df)[colnames(df) == "AS"] <- "secondaryAccession"
    colnames(df)[colnames(df) == "CA"] <- "category"
    colnames(df)[colnames(df) == "CC"] <- "comments"
    colnames(df)[colnames(df) == "DI"] <- "diseases"
    colnames(df)[colnames(df) == "DR"] <- "crossReferences"
    colnames(df)[colnames(df) == "DT"] <- "date"
    colnames(df)[colnames(df) == "HI"] <- "hierarchy"
    colnames(df)[colnames(df) == "ID"] <- "cellLineName"
    colnames(df)[colnames(df) == "OI"] <- "originateFromSameIndividual"
    colnames(df)[colnames(df) == "OX"] <- "speciesOfOrigin"
    colnames(df)[colnames(df) == "RX"] <- "referencesIdentifiers"
    colnames(df)[colnames(df) == "ST"] <- "strProfileData"
    colnames(df)[colnames(df) == "SX"] <- "sexOfCell"
    colnames(df)[colnames(df) == "SY"] <- "synonyms"
    colnames(df)[colnames(df) == "WW"] <- "webPages"
    assert(
        allAreMatchingRegex(x = df[["accession"]], pattern = "^CVCL_"),
        isCharacter(df[["cellLineName"]])
    )
    rownames(df) <- df[["accession"]]
    df <- df[order(rownames(df)), , drop = FALSE]
    metadata(df) <- list(
        "dataVersion" = dataVersion,
        "date" = Sys.Date(),
        "packageVersion" = .pkgVersion
    )
    df
}



#' Sanitize the `ageAtSampling` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeAgeAtSampling <- function(object) {
    assert(
        is(object, "DataFrame"),
        is.character(object[["ageAtSampling"]])
    )
    idx <- which(object[["ageAtSampling"]] == "Age unspecified")
    object[["ageAtSampling"]][idx] <- NA_character_
    object
}



#' Sanitize the `comments` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeComments <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "CharacterList")
    )
    object[["comments"]] <- unique(object[["comments"]])
    object[["comments"]] <- sub(
        pattern = "\\.$",
        replacement = "",
        x = object[["comments"]]
    )
    .splitNestedCol(object, colName = "comments", sep = ": ")
}



#' Sanitize the `crossReferences` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeCrossRefs <- function(object) {
    .splitNestedCol(
        object = object,
        colName = "crossReferences",
        sep = "; "
    )
}



#' Sanitize the `date` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeDate <- function(object) {
    .splitCol(object = object, colName = "date", split = "; ")
}



#' Sanitize the `diseases` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeDiseases <- function(object) {
    .splitNestedCol(object, colName = "diseases", sep = "; ")
}



#' Sanitize the `hierarchy` column
#'
#' @details
#' Some entries have multiple elements, such as "CVCL_0464".
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeHierarchy <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["hierarchy"]], "CharacterList")
    )
    lst <- CharacterList(lapply(
        X = object[["hierarchy"]],
        FUN = function(x) {
            spl <- stri_split_fixed(
                str = x,
                pattern = " ! ",
                n = 2L,
                simplify = TRUE
            )
            spl[, 1L]
        }
    ))
    object[["hierarchy"]] <- lst
    object
}



#' Sanitize the `referencesIdentifiers` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeRefIds <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["referencesIdentifiers"]], "CharacterList")
    )
    object[["referencesIdentifiers"]] <- sub(
        pattern = ";$",
        replacement = "",
        x = object[["referencesIdentifiers"]]
    )
    .splitNestedCol(
        object = object,
        colName = "referencesIdentifiers",
        sep = "="
    )
}



#' Sanitize the `synonyms` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeSynonyms <- function(object) {
    .splitCol(object = object, colName = "synonyms", split = "; ")
}



#' Split a column into a character list
#'
#' @note Updated 2023-01-23.
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



#' Split a nested column by key
#'
#' @note Updated 2023-01-24.
#' @noRd
.splitNestedCol <- function(object, colName, sep) {
    assert(
        is(object, "DataFrame"),
        is(object[[colName]], "CharacterList"),
        isString(sep)
    )
    lst <- SimpleList(lapply(
        X = object[[colName]],
        sep = sep,
        FUN = function(x, sep) {
            x <- stri_split_fixed(
                str = x,
                pattern = sep,
                n = 2L,
                simplify = TRUE
            )
            x <- split(x = x[, 2L], f = x[, 1L])
            x
        }
    ))
    object[[colName]] <- lst
    object
}



## Updated 2023-01-24.

#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function() {
        object <- .importCelloFromTxt()
        alert("Sanitizing annotations.")
        object <- .sanitizeAgeAtSampling(object)
        object <- .sanitizeComments(object)
        object <- .sanitizeCrossRefs(object)
        object <- .sanitizeDate(object)
        object <- .sanitizeDiseases(object)
        object <- .sanitizeHierarchy(object)
        object <- .sanitizeRefIds(object)
        object <- .sanitizeSynonyms(object)
        alert("Adding annotations.")
        object <- .addDepmapId(object)
        object <- .addIsCancer(object)
        object <- .addIsProblematic(object)
        object <- .addMsiStatus(object)
        object <- .addNcitDisease(object)
        object <- .addPopulation(object)
        object <- .addSamplingSite(object)
        object <- .addSangerModelId(object)
        object <- .addTaxonomy(object)
        object <- factorize(object)
        object <- encode(object)
        object <- object[, sort(colnames(object)), drop = FALSE]
        new("Cellosaurus", object)
    }
