## FIXME Consider renaming "id" to "cellosaurusId".



#' Cellosaurus table
#'
#' @export
#' @note Updated 2022-05-12.
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
Cellosaurus <- # nolint
    function() {
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
        ## Split `comment` column.
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
        df[["comment"]] <- CharacterList(strsplit(
            x = df[["comment"]],
            split = ". ",
            fixed = TRUE
        ))
        df[["comment"]] <- gsub(
            pattern = "\\.$",
            replacement = "",
            x = df[["comment"]]
        )
        ## Split `synonym` column.
        df[["synonym"]] <-
            gsub(
                pattern = " RELATED []",
                replacement = "",
                x = df[["synonym"]],
                fixed = TRUE,
            )
        df[["synonym"]] <-
            gsub(
                pattern = "\"",
                replacement = "",
                x = df[["synonym"]],
                fixed = TRUE,
            )
        df[["synonym"]] <- CharacterList(strsplit(
            x = df[["synonym"]],
            split = "; ",
            fixed = TRUE
        ))
        ## Split other semicolon-delimited columns.
        df[["originateFromSameIndividualAs"]] <-
            CharacterList(strsplit(
                x = df[["originateFromSameIndividualAs"]],
                split = "; ",
                fixed = TRUE
            ))
        df[["subset"]] <-
            CharacterList(strsplit(
                x = df[["subset"]],
                split = "; ",
                fixed = TRUE
            ))
        df[["xref"]] <-
            CharacterList(strsplit(
                x = df[["xref"]],
                split = "; ",
                fixed = TRUE
            ))

        # FIXME Add these additional columns:
        #
        # "isCancer" = any(str_detect(
        #     string = lines,
        #     pattern = "^CA[[:space:]]+Cancer cell line$"
        # )),
        # "isProblematic" = any(str_detect(
        #     string = lines,
        #     pattern = "^CC[[:space:]]+Problematic cell line.*$"
        # )),
        # "organism" = .strSubsetAndMatchSingle(
        #     string = lines,
        #     pattern = "^OX.+! (.+)$"
        # ),
        # "patientAgeYears" = as.integer(.strSubsetAndMatchSingle(
        #     string = lines,
        #     pattern = "^AG[[:space:]]+([0-9]+)Y$"
        # )),
        # "patientSex" = .strSubsetAndMatchSingle(
        #     string = lines,
        #     pattern = "^SX[[:space:]]+(.+)$"
        # )
        # assert(
        #     isString(out[["cellLineName"]]),
        #     isFlag(out[["isCancer"]]),
        #     isFlag(out[["isProblematic"]]),
        #     isOrganism(out[["organism"]])
        # )
        # if (identical(out[["organism"]], "Homo sapiens")) {
        #     assert(
        #         isInt(out[["patientAgeYears"]]) ||
        #             is.na(out[["patientAgeYears"]]),
        #         isSubset(out[["patientSex"]], c("Female", "Male"))
        #     )
        # }
        # ## Disease: NCIt.
        # ## Note that we're censoring problematic cell lines here.
        # match <- .strSubsetAndMatch(
        #     string = lines,
        #     pattern = "^DI[[:space:]]+NCIt; (C[0-9]+); (.+)$"
        # )
        # if (hasRows(match) && !isTRUE(out[["isProblematic"]])) {
        #     out[["ncitDiseaseId"]] <- match[1L, 2L]
        #     out[["ncitDiseaseName"]] <- match[1L, 3L]
        # } else {
        #     ## nocov start
        #     ncitDiseaseId <- NA_character_
        #     ncitDiseaseName <- NA_character_
        #     ## nocov end
        # }
        # ## Filter entries with "DR" tag.
        # dr <- str_match(
        #     string = lines,
        #     pattern = "^DR[[:space:]]+(.+); (.+)$"
        # )
        # dr <- dr[complete.cases(dr), c(2L, 3L), drop = FALSE]
        # assert(hasRows(dr))
        # colnames(dr) <- c("key", "value")
        # dr <- dr[order(dr[, "key"], dr[, "value"]), , drop = FALSE]
        # dr <- aggregate(
        #     x = formula("value~key"),
        #     data = dr,
        #     FUN = list
        # )

        metadata(df)[["dataVersion"]] <- dataVersion
        df <- df[, sort(colnames(df)), drop = FALSE]
        new("CellosaurusTable", df)
    }
