#' Cellosaurus table
#'
#' @name Cellosaurus
#' @note Updated 2023-09-22.
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



#' Add `atccId` column
#'
#' @note Updated 2023-09-01.
#' @noRd
.addAtccId <- function(object) {
    object[["atccId"]] <- .extractCrossRef(
        object = object,
        keyName = "ATCC"
    )
    object
}



#' Add `depmapId` column
#'
#' @details
#' Note that some cell lines, such as "CVCL_0041", map to multiple DepMap
#' identifiers, which is confusing.
#'
#' @note Updated 2023-09-01.
#' @noRd
.addDepmapId <- function(object) {
    object[["depmapId"]] <- .extractCrossRef(
        object = object,
        keyName = "DepMap"
    )
    object
}



#' Add `isCancer` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addIsCancer <- function(object) {
    assert(
        is(object, "DFrame"),
        isCharacter(object[["category"]])
    )
    lgl <- object[["category"]] == "Cancer cell line"
    object[["isCancer"]] <- lgl
    object
}



#' Add `isContaminated` column
#'
#' @note Updated 2023-08-24.
#' @noRd
.addIsContaminated <- function(object) {
    assert(
        is(object, "DFrame"),
        is(object[["comments"]], "SimpleList")
    )
    lgl <- vapply(
        X = object[["comments"]],
        FUN = function(x) {
            if (!"Problematic cell line" %in% names(x)) {
                return(FALSE)
            }
            x <- x[["Problematic cell line"]]
            any(grepl(pattern = "^Contaminated.", x = x))
        },
        FUN.VALUE = logical(1L)
    )
    object[["isContaminated"]] <- lgl
    object
}



#' Add `isProblematic` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addIsProblematic <- function(object) {
    assert(
        is(object, "DFrame"),
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



#' Add `misspellings` column
#'
#' @note Updated 2023-09-01.
#' @noRd
.addMisspellings <- function(object) {
    vals <- .extractComment(object = object, keyName = "Misspelling")
    vals <- sub(pattern = "^([^;]+);.+$", replacement = "\\1", x = vals)
    object[["misspellings"]] <- vals
    object
}



#' Add `msiStatus` column
#'
#' @note Updated 2023-09-01.
#' @noRd
.addMsiStatus <- function(object) {
    object[["msiStatus"]] <- .extractComment(
        object = object,
        keyName = "Microsatellite instability"
    )
    object
}



#' Extract NCI thesaurus disease identifiers
#'
#' @details
#' Some cell lines rarely map to multiple identifiers, such as "CVCL_0028".
#'
#' @note Updated 2023-09-22.
#' @noRd
.addNcitDisease <- function(object) {
    assert(
        is(object, "DFrame"),
        is(object[["diseases"]], "SimpleList")
    )
    spl <- mclapply(
        X = object[["diseases"]],
        FUN = function(x) {
            x <- x[["NCIt"]]
            if (is.null(x)) {
                return(NULL)
            }
            x <- strSplit(x = x, split = "; ")
            x
        }
    )
    id <- CharacterList(lapply(
        X = spl,
        FUN = function(x) {
            x[, 1L]
        }
    ))
    name <- CharacterList(lapply(
        X = spl,
        FUN = function(x) {
            x[, 2L]
        }
    ))
    object[["ncitDiseaseId"]] <- id
    object[["ncitDiseaseName"]] <- name
    object
}



#' Add OntoTree metadata columns
#'
#' @note Updated 2023-08-26.
#' @noRd
.addOncotree <- function(object) {
    assert(
        is(object, "DFrame"),
        is(ncit2Oncotree, "DFrame"),
        is(oncotree, "DFrame"),
        isSubset(c("accession", "ncitDiseaseId"), colnames(object)),
        is(object[["ncitDiseaseId"]], "CharacterList"),
        identical(
            x = colnames(ncit2Oncotree),
            y = c("ncit", "oncotree")
        ),
        identical(
            x = colnames(oncotree),
            y = c(
                "code",
                "color",
                "name",
                "mainType",
                "externalReferences",
                "tissue",
                "parent",
                "history",
                "level",
                "revocations",
                "precursors"
            )
        )
    )
    ## Build our data frame that we will use for the left join.
    x <- object[, c("accession", "ncitDiseaseId")]
    ## Ignore any cell lines with multi-mapped NCIt terms.
    keep <- bapply(
        X = x[["ncitDiseaseId"]],
        FUN = function(x) {
            identical(length(x), 1L)
        }
    )
    x <- x[keep, , drop = FALSE]
    x[["ncitDiseaseId"]] <- unlist(
        x = x[["ncitDiseaseId"]],
        recursive = FALSE,
        use.names = FALSE
    )
    ## Join the NCIt-to-OncoTree mappings.
    y <- ncit2Oncotree
    colnames(y) <- c("ncitDiseaseId", "oncotreeCode")
    x <- leftJoin(x = x, y = y, by = "ncitDiseaseId")
    # Join the OncoTree metadata.
    y <- oncotree
    rownames(y) <- NULL
    y <- y[
        ,
        setdiff(
            x = colnames(y),
            y = c(
                "color",
                "externalReferences",
                "history",
                "precursors",
                "revocations"
            )
        )
    ]
    colnames(y) <- camelCase(paste("oncotree", colnames(y)))
    x <- leftJoin(x = x, y = y, by = "oncotreeCode")
    x[["ncitDiseaseId"]] <- NULL
    out <- leftJoin(x = object, y = x, by = "accession")
    out
}



#' Add `population` column
#'
#' @note Updated 2023-09-01.
#' @noRd
.addPopulation <- function(object) {
    object[["population"]] <- .extractComment(
        object = object,
        keyName = "Population"
    )
    object
}



#' Add `samplingSite` column
#'
#' @details
#' Note that some comments have additional information on the cell type, that
#' we don't want to include here (e.g. CVCL_0003, CVCL_0008).
#'
#' @note Updated 2023-09-01.
#' @noRd
.addSamplingSite <- function(object) {
    object[["samplingSite"]] <- .extractComment(
        object = object,
        keyName = "Derived from site"
    )
    object
}



#' Add `sangerModelId` column
#'
#' @note Updated 2023-09-01.
#' @noRd
.addSangerModelId <- function(object) {
    object[["sangerModelId"]] <- .extractCrossRef(
        object = object,
        keyName = "Cell_Model_Passport"
    )
    object
}



#' Add `ncbiTaxonomyId` and `organism` columns
#'
#' @note Updated 2023-09-22.
#' @noRd
#'
#' @details
#' Note that hybrid lines contain multiple taxonomies, so we want to keep this
#' as a list instead of attempting to coerce to a vector.
#'
#' Examples: CVCL_0464, CVCL_0E28, CVCL_0E29.
.addTaxonomy <- function(object) {
    assert(
        is(object, "DFrame"),
        is(object[["speciesOfOrigin"]], "CharacterList")
    )
    spl <- mclapply(
        X = object[["speciesOfOrigin"]],
        FUN = strSplit,
        split = "; ! ",
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
    ## Standardize organism to full Latin name. For example, this step converts
    ## "Homo sapiens (Human)" to simply "Homo sapiens".
    organism <- sub(
        pattern = "\\s\\(.+\\)$",
        replacement = "",
        x = organism
    )
    object[["ncbiTaxonomyId"]] <- taxId
    object[["organism"]] <- organism
    object[["speciesOfOrigin"]] <- NULL
    object
}



#' Extract and assign identifier column from `crossReferences`
#'
#' @details
#' Note that this intentionally picks only the last matching identifier.
#'
#' This helps avoid issues with discontinued identifiers, such as CVCL_0455,
#' which has multiple DepMap identifiers: ACH-000474 - Discontinued; ACH-001075.
#'
#' @note Updated 2023-09-01.
#' @noRd
.extractCrossRef <- function(object, keyName) {
    assert(
        is(object, "DFrame"),
        is(object[["crossReferences"]], "SimpleList"),
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
            ids[[1L]]
        },
        FUN.VALUE = character(1L)
    )
    x
}



#' Extract a key value pair from comments
#'
#' @note Updated 2023-09-01.
#' @noRd
.extractComment <- function(object, keyName) {
    assert(
        is(object, "DFrame"),
        is(object[["comments"]], "SimpleList"),
        isString(keyName)
    )
    x <- object[["comments"]]
    x <- CharacterList(lapply(
        X = x,
        FUN = function(x) {
            if (is.null(x[[keyName]])) {
                return(NULL)
            }
            x[[keyName]]
        }
    ))
    x
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
#' @note Updated 2023-08-25.
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
    lines <- import(con, format = "lines")
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
    assert(isString(dataVersion))
    dataVersion <- as.numeric_version(dataVersion)
    alert(sprintf(
        "Detected Cellosaurus release %s.",
        as.character(dataVersion)
    ))
    alert(sprintf("Mapping lines into {.cls %s} of entries.", "list"))
    x <- Map(
        f = function(lines, i, j) {
            lines[i:(j - 1L)]
        },
        i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
        j = grep(pattern = "^//$", x = lines, value = FALSE),
        MoreArgs = list("lines" = lines),
        USE.NAMES = FALSE
    )
    requiredKeys <- c("AC", "CA", "DT", "ID")
    nestedKeys <- c("CC", "DI", "DR", "HI", "OI", "OX", "RX", "ST", "WW")
    optionalKeys <- c("AG", "AS", "SX", "SY")
    alert("Processing entries.")
    x <- mclapply(
        X = x,
        FUN = .processEntry,
        nestedKeys = nestedKeys,
        optionalKeys = optionalKeys
    )
    alert(sprintf(
        "Coercing entries {.cls %s} to {.cls %s} with {.pkg %s}::{.fun %s}.",
        "list", "DFrame", "AcidPlyr", "rbindToDataFrame"
    ))
    df <- rbindToDataFrame(x)
    assert(areSetEqual(
        x = colnames(df),
        y = c(requiredKeys, optionalKeys, nestedKeys)
    ))
    for (key in requiredKeys) {
        assert(isCharacter(df[[key]]))
    }
    for (key in optionalKeys) {
        assert(is.character(df[[key]]))
    }
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



#' Process lines per entry
#'
#' @note Updated 2023-09-21.
#' @noRd
.processEntry <- function(x, nestedKeys, optionalKeys) {
    x <- strSplit(x, split = "   ")
    x <- split(x[, 2L], f = x[, 1L])
    for (key in optionalKeys) {
        if (is.null(x[[key]])) {
            x[[key]] <- NA_character_
        }
    }
    for (key in nestedKeys) {
        x[[key]] <- unique(x[[key]])
    }
    ## Filter out discontinued identifiers from DR (e.g. "CVCL_0455").
    discontinued <- grep(
        pattern = "Discontinued:",
        x = x[["CC"]],
        fixed = TRUE,
        value = TRUE
    )
    if (isTRUE(length(discontinued) > 0L)) {
        discontinued <- sub(
            pattern = "^Discontinued: (.+);.+$",
            replacement = "\\1",
            x = discontinued
        )
        x[["DR"]] <- setdiff(x = x[["DR"]], y = discontinued)
    }
    x
}



#' Sanitize the `ageAtSampling` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeAgeAtSampling <- function(object) {
    assert(
        is(object, "DFrame"),
        is.character(object[["ageAtSampling"]])
    )
    idx <- which(object[["ageAtSampling"]] == "Age unspecified")
    object[["ageAtSampling"]][idx] <- NA_character_
    object
}



#' Sanitize the `comments` column
#'
#' @note Updated 2023-09-22.
#' @noRd
.sanitizeComments <- function(object) {
    assert(
        is(object, "DFrame"),
        is(object[["comments"]], "CharacterList")
    )
    object[["comments"]] <- unique(object[["comments"]])
    object[["comments"]] <- sub(
        pattern = "\\.$",
        replacement = "",
        x = object[["comments"]]
    )
    .splitNestedCol(object, colName = "comments", split = ": ")
}



#' Sanitize the `crossReferences` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeCrossRefs <- function(object) {
    .splitNestedCol(
        object = object,
        colName = "crossReferences",
        split = "; "
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
    .splitNestedCol(object, colName = "diseases", split = "; ")
}



## FIXME Need to handle empty character here.

#' Sanitize the `hierarchy` column
#'
#' @details
#' Some entries have multiple elements, such as "CVCL_0464".
#'
#' @note Updated 2023-09-22.
#' @noRd
.sanitizeHierarchy <- function(object) {
    assert(
        is(object, "DFrame"),
        is(object[["hierarchy"]], "CharacterList")
    )
    lst <- mclapply(
        X = object[["hierarchy"]],
        FUN = function(x) {
            if (identical(x, character())) {
                return(character())
            }
            spl <- strSplit(x, split = " ! ")
            out <- spl[, 1L]
            out
        }
    )
    lst <- CharacterList(lst)
    object[["hierarchy"]] <- lst
    object
}



#' Sanitize the `referencesIdentifiers` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeRefIds <- function(object) {
    assert(
        is(object, "DFrame"),
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
        split = "="
    )
}



#' Sanitize the `strProfileData` column
#'
#' @note Updated 2023-01-31.
#' @noRd
.sanitizeStrProfileData <- function(object) {
    .splitNestedCol(object = object, colName = "strProfileData", split = ": ")
}



## FIXME This is hitting a non-character argument.

#' Sanitize the `synonyms` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.sanitizeSynonyms <- function(object) {
    .splitCol(object = object, colName = "synonyms", split = "; ")
}


## FIXME This step is erroring for sanitizeComments, may need to rework.

#' Split a column into a character list
#'
#' @note Updated 2023-09-22.
#' @noRd
.splitCol <- function(object, colName, split = "; ") {
    assert(
        is(object, "DFrame"),
        isString(colName),
        isString(split)
    )
    spl <- strsplit(
        x = object[[colName]],
        split = split,
        fixed = TRUE
    )
    cl <- CharacterList(spl)
    object[[colName]] <- cl
    object
}



#' Split a nested column by key
#'
#' @note Updated 2023-09-21.
#' @noRd
.splitNestedCol <- function(object, colName, split) {
    assert(
        is(object, "DFrame"),
        is(object[[colName]], "CharacterList"),
        isString(split)
    )
    lst <- mclapply(
        X = object[[colName]],
        split = split,
        FUN = function(x, split) {
            if (identical(x, character())) {
                return(list())
            }
            x <- strSplit(x = x, split = split, n = 2L)
            x <- split(x = x[, 2L], f = x[, 1L])
            x
        }
    )
    lst <- SimpleList(lst)
    object[[colName]] <- lst
    object
}



## Updated 2023-09-22.

#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function() {
        ## Benchmark: ~1 minute, 15 seconds.
        object <- .importCelloFromTxt()
        alert("Sanitizing annotations.")
        object <- .sanitizeAgeAtSampling(object)
        ## Benchmark: ~1 minute.
        object <- .sanitizeComments(object)
        ## Benchmark: ~1 minute, 15 seconds.
        object <- .sanitizeCrossRefs(object)
        object <- .sanitizeDate(object)
        ## Benchmark: ~25 seconds.
        object <- .sanitizeDiseases(object)
        ## Benchmark: ~15 seconds.
        object <- .sanitizeHierarchy(object)
        ## Benchmark: ~30 seconds.
        object <- .sanitizeRefIds(object)
        object <- .sanitizeStrProfileData(object)
        object <- .sanitizeSynonyms(object)
        alert("Adding annotations.")
        object <- .addAtccId(object)
        object <- .addDepmapId(object)
        object <- .addIsCancer(object)
        object <- .addIsContaminated(object)
        object <- .addIsProblematic(object)
        object <- .addMisspellings(object)
        object <- .addMsiStatus(object)
        ## Benchmark: ~40 seconds.
        object <- .addNcitDisease(object)
        object <- .addOncotree(object)
        object <- .addPopulation(object)
        object <- .addSamplingSite(object)
        object <- .addSangerModelId(object)
        ## Benchmark: ~1 minute, 15 seconds.
        object <- .addTaxonomy(object)
        alert("Encoding metadata.")
        object <- encode(object)
        object <- object[, sort(colnames(object)), drop = FALSE]
        new("Cellosaurus", object)
    }
