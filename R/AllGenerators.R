#' Cellosaurus table of cell identifier mappings
#'
#' @export
#' @note Updated 2020-10-01.
#'
#' @inheritParams AcidRoxygen::params
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
    function(
        x,
        BPPARAM = BiocParallel::bpparam()  # nolint
    ) {
        assert(
            isCharacter(x),
            allAreMatchingFixed(x, "CVCL_")
        )
        list <- bplapply(
            X = x,
            FUN = function(id) {
                url <- paste0("https://web.expasy.org/cellosaurus/", id, ".txt")
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
                ## Prepare data frame.
                out <- data.frame(
                    "cellLineName" = cellLineName,
                    "cellosaurusId" = id,
                    "isCancer" = isCancer,
                    "isProblematic" = isProblematic,
                    "ncitDiseaseId" = ncitDiseaseId,
                    "ncitDiseaseName" = ncitDiseaseName,
                    "patientSex" = patientSex,
                    "patientAgeYears" = patientAgeYears
                )
                ## Filter entries with "DR" tag.
                dr <- str_match(
                    string = lines,
                    pattern = "^DR[[:space:]]+(.+); (.+)$"
                )
                dr <- dr[complete.cases(dr), c(2L, 3L), drop = FALSE]
                colnames(dr) <- c("key", "value")
                dr <- as.data.frame(dr)
                keep <- dr[[1L]] %in% c(
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
                dr <- arrange_all(dr)
                dr <- group_by(dr, !!sym("key"))
                dr <- summarize(
                    dr,
                    value = toString(!!sym("value")),
                    .groups = "keep"
                )
                dr <- ungroup(dr)
                dr <- as.data.frame(dr)
                dr <- column_to_rownames(dr, "key")
                dr <- t(dr)
                rownames(dr) <- NULL
                out <- cbind(out, dr)
                colnames(out) <- camelCase(colnames(out), strict = TRUE)
                out
            },
            BPPARAM = BPPARAM
        )
        data <- data.table::rbindlist(list, fill = TRUE)
        data <- as(data, "DataFrame")
        rownames(data) <- data[["cellosaurusId"]]
        new("CellosaurusTable", data)
    }
