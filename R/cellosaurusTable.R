#' Cellosaurus metadata table
#'
#' @export
#' @note Updated 2020-08-02.
#'
#' @param x `character`.
#'   Cellosaurus identifiers (e.g. `CVCL_*`.)
#'
#' @examples
#' cellosaurusTable("CVCL_1045")
cellosaurusTable <- function(
    x,
    BPPARAM = BiocParallel::bpparam()
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
            cellLineName <- .str_subset_and_match_single(
                string = head(lines, n = 1L),
                pattern = "^<pre>ID[[:space:]]+(.+)$"
            )
            assert(isString(cellLineName))
            ## Disease: NCIt.
            match <- .str_subset_and_match(
                string = lines,
                pattern = "^DI[[:space:]]+NCIt; (C[0-9]+); (.+)$"
            )
            if (hasRows(match)) {
                ncitDiseaseID <- match[1L, 2L]
                ncitDiseaseName <- match[1L, 3L]
            } else {
                ncitDiseaseID <- NULL
                ncitDiseaseName <- NULL
            }
            ## Organism.
            organism <- .str_subset_and_match_single(
                string = lines,
                pattern = "^OX.+! (.+)$"
            )
            ## Patient age.
            patientAgeYears <- .str_subset_and_match_single(
                string = lines,
                pattern = "^AG[[:space:]]+([0-9]+)Y$"
            )
            ## Patient sex.
            patientSex <- .str_subset_and_match_single(
                string = lines,
                pattern = "^SX[[:space:]]+(.+)$"
            )
            ## Is the cell line a cancer cell?
            isCancer <- any(str_detect(
                string = lines,
                pattern = "^CA[[:space:]]+Cancer cell line$"
            ))
            ## Prepare tibble.
            out <- tibble(
                cellLineName = cellLineName,
                cellosaurusID = id,
                ncitDiseaseID = ncitDiseaseID,
                ncitDiseaseName = ncitDiseaseName,
                patientAgeYears = patientAgeYears,
                patientSex = patientSex,
                isCancer = isCancer
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
                "IGRhCellID",
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
            out <- camelCase(out)
            out
        },
        BPPARAM = BPPARAM
    )


}
