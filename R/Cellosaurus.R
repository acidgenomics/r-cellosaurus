#' Cellosaurus table
#'
#' @name Cellosaurus
#' @note Updated 2023-01-20.
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



## FIXME Need to rework this.

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



## FIXME Need to rework this.

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



## FIXME Need to rework this.

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



## FIXME Need to rework this.

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



## FIXME Need to rework this.

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



#' Extract NCI thesaurus disease identifiers
#'
#' @details
#' Some cell lines rarely map to multiple identifiers, such as "CVCL_0028".
#'
#' @note Updated 2023-01-20.
#' @noRd
.addNcitDisease <- function(object) {
    alert("Adding NCIt disease metadata.")
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



## FIXME Need to rework this.

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



## FIXME Need to rework this.

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



#' Extract and add NCBI taxonomy identifiers, organism
#'
#' @note Updated 2023-01-20.
#' @noRd
.addTaxonomy <- function(object) {
    alert("Adding NCBI taxonomy identifier and organism metadata.")
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



## FIXME Need to rework this.

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
#' @note Updated 2023-01-20.
#' @noRd
#'
#' @seealso
#' - https://github.com/calipho-sib/cellosaurus/
#' - https://ftp.expasy.org/databases/cellosaurus/
.importCelloTxt <- function() {
    url <- pasteURL(
        "r.acidgenomics.com",
        "extdata",
        "cellosaurus",
        paste0("cellosaurus-", .release, ".txt"),
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
    alert("Processing entries (CPU intensive).")
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
    plan(strategy = multisession)
    x <- future_lapply(
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



#' Split a column into a character list
#'
#' @note Updated 2023-01-19.
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



## Updated 2023-01-20.

#' @rdname Cellosaurus
#' @export
Cellosaurus <- # nolint
    function() {
        object <- .importCelloTxt()
        assert(allAreMatchingRegex(x = rownames(object), pattern = "^CVCL_"))
        object <- object[order(rownames(object)), ]
        assert(isCharacter(object[["cellLineName"]]))
        object <- .splitCol(object, colName = "date", split = "; ")
        object <- .splitCol(object, colName = "synonyms", split = "; ")
        object <- .addTaxonomy(object)
        object <- .addNcitDisease(object)

        ## FIXME Current state of progress.
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
        metadata(object)[["packageVersion"]] <- .pkgVersion
        new("Cellosaurus", object)
    }
