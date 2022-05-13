## FIXME Need to be able to match NCBI taxonomy identifiers using API.
## FIXME Need to be able to match NCIt identifiers using API.



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
        ## Extract DepMap identifiers.
        depMapId <- grep(
            pattern = "^DepMap:",
            x = df[["xref"]],
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
        df[["depMapId"]] <- depMapId
        ## Label any problematic cell lines.
        df[["isProblematic"]] <- any(grepl(
            pattern = "Problematic cell line",
            x = df[["comment"]],
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
        df[["isCancer"]] <- any(df[["subset"]] == "Cancer_cell_line")
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



        # ## Disease: NCIt.
        # FIXME e.g. "NCIt:C7152"
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
