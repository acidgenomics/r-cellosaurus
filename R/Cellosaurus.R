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
#' @note Updated 2023-01-23.
#' @noRd
.addDepmapId <- function(object) {
    .extractCrossRef(
        object,
        colName = "depmapId",
        keyName = "DepMap"
    )
}



#' Add `ethnicity` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addEthnicity <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "CharacterList")
    )
    x <- object[["comments"]]
    x <- grep(pattern = "^Population:", x = x, value = TRUE)
    x <- sub(pattern = "^Population: ", replacement = "", x = x)
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
        is(object[["comments"]], "CharacterList")
    )
    lgl <- any(grepl(
        pattern = "Problematic cell line",
        x = object[["comments"]],
        fixed = TRUE
    ))
    object[["isProblematic"]] <- lgl
    object
}



#' Add `msiStatus` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.addMsiStatus <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "CharacterList")
    )
    x <- object[["comments"]]
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
        is(object[["diseases"]], "CharacterList")
    )
    spl <- lapply(
        X = object[["diseases"]],
        FUN = function(x) {
            lgl <- grepl(
                pattern = "NCIt",
                x = x,
                ignore.case = FALSE,
                fixed = TRUE
            )
            if (!any(lgl)) {
                return(NULL)
            }
            x <- x[lgl]
            x <- sub(
                pattern = "NCIt; ",
                replacement = "",
                x = x,
                ignore.case = FALSE,
                fixed = TRUE
            )
            x <- stri_split_fixed(
                str = x,
                pattern = "; ",
                n = 2L,
                simplify = TRUE
            )
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



#' Add `samplingSite` column
#'
#' @details
#' Note that some comments have additional information on the cell type, that
#' we don't want to include here (e.g. CVCL_0003).
#'
#' CVCL_C4RX currently has a duplication of colon.
#'
#' @note Updated 2023-01-24.
#' @noRd
.addSamplingSite <- function(object) {
    assert(
        is(object, "DataFrame"),
        is(object[["comments"]], "CharacterList")
    )
    x <- object[["comments"]]
    x <- grep(pattern = "^Derived from sampling site: ", x = x, value = TRUE)
    x <- sub(pattern = "\\.\\s.+", replacement = "", x = x)
    x <- sub(pattern = "^Derived from sampling site: ", replacement = "", x = x)
    assert(identical(length(x), nrow(object)))
    object[["samplingSite"]] <- x
    object
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
#' @note Updated 2023-01-23.
#' @noRd
.extractCrossRef <- function(object, colName, keyName, sep = "; ") {
    assert(
        is(object, "DataFrame"),
        isString(colName),
        isString(keyName),
        isString(sep)
    )
    x <- vapply(
        X = object[["crossReferences"]],
        keyName = keyName,
        sep = sep,
        FUN = function(x, keyName, sep) {
            ids <- grep(
                pattern = paste0("^", keyName, sep),
                x = x,
                ignore.case = FALSE,
                value = TRUE
            )
            if (identical(ids, character())) {
                return(NA_character_)
            }
            ## Return only the first match.
            ids[[1L]]
        },
        FUN.VALUE = character(1L)
    )
    ## This `sub` approach handles NA better than using `stri_split_fixed`.
    x <- sub(
        pattern = paste0("^", keyName, sep),
        replacement = "",
        x = x
    )
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
    alert("Mapping lines into list of entries.")
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
            x[[key]] <- list(x[[key]])
        }
        x
    }
    if (isTRUE(requireNamespace("future.apply", quietly = TRUE))) {
        alert(sprintf(
            "Processing entries in parallel with {.pkg %s}.",
            "future.apply"
        ))
        future::plan(strategy = future::multisession)
        .lapply <- future.apply::future_lapply
    } else {
        alert(sprintf(
            paste(
                "Processing entries single threaded.",
                "Install {.pkg %s} to speed up this step."
            ),
            "future.apply"
        ))
    }
    x <- .lapply(
        X = x,
        FUN = .processEntry,
        nestedKeys = nestedKeys,
        optionalKeys = optionalKeys
    )
    alert("Converting entries list to data frame.")
    ## Alternatively, can use `AcidPlyr::mapToDataFrame` here, but is slower.
    df <- rbindlist(l = x, use.names = TRUE, fill = FALSE)
    df <- as(df, "DataFrame")
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
    rownames(df) <- df[["accession"]]
    metadata(df)[["dataVersion"]] <- dataVersion
    df
}



#' Sanitize the hierarchy column
#'
#' @details
#' Some entries have multiple elements, such as "CVCL_0464".
#'
#' @note Updated 2023-01-20.
#' @noRd
.sanitizeHierarchy <- function(object) {
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



## Updated 2023-01-24.

#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function() {
        df <- .importCelloFromTxt()
        assert(
            allAreMatchingRegex(x = rownames(df), pattern = "^CVCL_"),
            isCharacter(df[["cellLineName"]])
        )
        ## Coerce list columns to CharacterList.
        for (col in c(
            "comments",
            "crossReferences",
            "diseases",
            "hierarchy",
            "originateFromSameIndividual",
            "referencesIdentifiers",
            "speciesOfOrigin",
            "strProfileData",
            "webPages"
        )) {
            assert(is.list(df[[col]]))
            df[[col]] <- CharacterList(df[[col]])
        }
        df <- df[order(rownames(df)), ]
        alert("Processing additional metadata.")
        ## Remove trailing period from comments.
        ## FIXME Need to remove any duplicated comments (e.g. CVCL_C4RX).
        df[["comments"]] <- sub(
            pattern = "\\.$",
            replacement = "",
            x = df[["comments"]]
        )
        df <- .splitCol(df, colName = "date", split = "; ")
        df <- .splitCol(df, colName = "synonyms", split = "; ")
        df <- .addDepmapId(df)
        df <- .addSangerModelId(df)
        df <- .addNcitDisease(df)
        df <- .addTaxonomy(df)
        df <- .addEthnicity(df)
        df <- .addIsCancer(df)
        df <- .addIsProblematic(df)
        df <- .addMsiStatus(df)
        df <- .addSamplingSite(df)
        df <- .sanitizeHierarchy(df)
        df <- factorize(df)
        df <- encode(df)
        df <- df[, sort(colnames(df)), drop = FALSE]
        metadata(df)[["packageVersion"]] <- .pkgVersion
        new("Cellosaurus", df)
    }
