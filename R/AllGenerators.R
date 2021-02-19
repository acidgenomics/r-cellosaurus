#' Cellosaurus table of cell identifier mappings
#'
#' @export
#' @note Updated 2021-02-19.
#'
#' @param x `character`.
#'   Cellosaurus identifiers (e.g. `CVCL_*`.)
#'
#' @return `DataFrame`.
#'
#' @examples
#' ids <- c("CVCL_0126", "CVCL_1045")
#' object <- CellosaurusTable(ids)
#' print(object)
CellosaurusTable <-  # nolint
    function(x) {
        assert(
            isCharacter(x),
            allAreMatchingFixed(x, "CVCL_")
        )
        list <- lapply(
            X = x,
            FUN = function(id) {
                url <- pasteURL(
                    "web.expasy.org",
                    "cellosaurus",
                    paste0(id, ".txt"),
                    protocol = "https"
                )
                response <- GET(url)
                content <- content(response, as = "text")
                lines <- strsplit(content, split = "\n")[[1L]]
                ## Cell line name.
                cellLineName <- .strSubsetAndMatchSingle(
                    string = head(lines, n = 1L),
                    pattern = "^<pre>ID[[:space:]]+(.+)$"
                )
                assert(isString(cellLineName))
                ## Is the cell line problematic?
                isProblematic <- any(str_detect(
                    string = lines,
                    pattern = "^CC[[:space:]]+Problematic cell line.*$"
                ))
                ## Disease: NCIt.
                ## Note that we're censoring problematic cell lines here.
                match <- .strSubsetAndMatch(
                    string = lines,
                    pattern = "^DI[[:space:]]+NCIt; (C[0-9]+); (.+)$"
                )
                if (hasRows(match) && !isTRUE(isProblematic)) {
                    ncitDiseaseId <- match[1L, 2L]
                    ncitDiseaseName <- match[1L, 3L]
                } else {
                    ## nocov start
                    ncitDiseaseId <- NA
                    ncitDiseaseName <- NA
                    ## nocov end
                }
                ## Organism.
                organism <- .strSubsetAndMatchSingle(
                    string = lines,
                    pattern = "^OX.+! (.+)$"
                )
                ## Patient age.
                patientAgeYears <- .strSubsetAndMatchSingle(
                    string = lines,
                    pattern = "^AG[[:space:]]+([0-9]+)Y$"
                )
                ## Patient sex.
                patientSex <- .strSubsetAndMatchSingle(
                    string = lines,
                    pattern = "^SX[[:space:]]+(.+)$"
                )
                ## Is the cell line a cancer cell?
                isCancer <- any(str_detect(
                    string = lines,
                    pattern = "^CA[[:space:]]+Cancer cell line$"
                ))
                ## Filter entries with "DR" tag.
                dr <- str_match(
                    string = lines,
                    pattern = "^DR[[:space:]]+(.+); (.+)$"
                )
                dr <- dr[complete.cases(dr), c(2L, 3L), drop = FALSE]
                colnames(dr) <- c("key", "value")
                keep <- dr[, 1L] %in% c(
                    ## Cell line databases / resource.
                    "CCLE",
                    "Cell_Model_Passport",
                    "Cosmic-CLP",
                    "DepMap",
                    "LINCS_HMS",
                    "LINCS_LDP",
                    ## Ontologies:
                    "BTO",
                    "CLO",
                    "EFO",
                    "MCCL"
                )
                dr <- dr[keep, , drop = FALSE]
                dr <- dr[order(dr[, "key"], dr[, "value"]), , drop = FALSE]
                dr <- aggregate(
                    formula = formula("value~key"),
                    data = dr,
                    FUN = list
                )
                assert(is.data.frame(dr))
                dr2 <- dr[["value"]]
                names(dr2) <- dr[["key"]]
                out <- list(
                    "cellLineName" = cellLineName,
                    "cellosaurusId" = id,
                    "isCancer" = isCancer,
                    "isProblematic" = isProblematic,
                    "ncitDiseaseId" = ncitDiseaseId,
                    "ncitDiseaseName" = ncitDiseaseName,
                    "patientSex" = patientSex,
                    "patientAgeYears" = patientAgeYears
                )
                out <- append(x = out, values = dr2)
                names(out) <- camelCase(names(out), strict = TRUE)
                out
            }
        )
        names(list) <- x
        ## Alternatively, `rbindlist(l = list, fill = TRUE)` is a useful
        ## approach to consider, but it doesn't return 1:1 on the rows.

        ## FIXME Need to rework `as.DataFrame()` to handle this??

        ## FIXME THIS IS CREATING DUPLICATE LINES ARGH....
        ## FIXME THIS FUNCTION IS MESSED UP AND NEEDS A REWORK IN PIPETTE...
        ## FIXME rbindlist is also doing this too, what's up with that.
        ## FIXME ITS BECAUSE CLO HAS DUPLICATES HERE...NEED TO USE I() HERE...
        xxx <- unlistToDataFrame(list)
        ## FIXME CREATE DATA FRAME AT LAST STEP...
        ## FIXME Can we switch to `unlistToDataFrame` here?


        data <- as(data, "DataFrame")
        rownames(data) <- data[["cellosaurusId"]]
        new("CellosaurusTable", data)
    }
