## FIXME Need to rework this.

## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(S4Vectors)
    library(goalie)
    library(pipette)
    library(AcidPlyr) # 0.4.0
    library(DepMapAnalysis) # 0.3.0.9000
})
## nolint end
load_all()

## NCIt to OncoTree mappings ===================================================

## NCI thesaurus
##   https://ncithesaurus.nci.nih.gov/
## OncoTree
##   https://oncotree.mskcc.org/

## cBioPortal oncotree mappings file.
df1 <- import(
    con = pasteURL(
        "raw.githubusercontent.com",
        "cBioPortal",
        "oncotree",
        "master",
        "scripts",
        "ontology_to_ontology_mapping_tool",
        "ontology_mappings.txt",
        protocol = "https"
    ),
    format = "tsv"
)
df1 <- as(df1, "DFrame")
df1 <- df1[, c("NCIT_CODE", "ONCOTREE_CODE")]
colnames(df1) <- c("ncit", "oncotree")
df1 <- df1[complete.cases(df1), ]
df1 <- unique(df1)
## e.g. "C9168,C7967" for "SRCCR".
df1[["ncit"]] <- strsplit(df1[["ncit"]], split = ",")
df1 <- unnest2(df1, col = "ncit")
df1 <- unique(df1)
df1 <- df1[order(df1[["ncit"]]), ]
## This file has NCIt identifiers mapping to multiple OncoTree codes, which is
## annoying. Drop any of these duplicate cases.
dupes <- df1[["ncit"]][which(duplicated(df1[["ncit"]]))]
## > print(dupes)
##  [1] "C27349" "C27893" "C3058"  "C3768"  "C40090" "C40208" "C40364" "C41247"
##  [9] "C4221"  "C4290"  "C4519"  "C4906"  "C6344"  "C7151"  "C7724"  "C8106"
## [17] "C82616" "C9245"  "C9296"  "C9461"
df1 <- df1[!df1[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df1[["ncit"]]))
df1[["source"]] <- "cbioportal"

## Can't use the DepMap "Model.csv" file directly, as it doesn't contain any
## NCIt mappings. It only contains RRID identifiers for Cellosaurus.
df2 <- DepMapAnalysis:::.importBroadModelInfo()
## > print(nrow(df2))
## 1824
df2 <- DataFrame(
    "ncit" = df2[["cellosaurus"]][["ncitDiseaseId"]],
    "oncotree" = df2[["broad"]][["OncotreeCode"]]
)
df2 <- df2[!is.na(df2[["oncotree"]]), ]
df2 <- unique(df2)
## Some of these terms are nested: C3798,C3273.
## FIXME How to unnest this without tidyverse?
## > tidyr::unnest(tibble::as_tibble(df2), cols = "ncit")


map <- rbind(df1, df2)
map <- map[, c("ncit", "oncotree")]
map <- unique(map)
## FIXME Assert that all NCIT codes match expectations.
## FIXME Assert that all OncoTree codes match expections.
assert(
    hasNoDuplicates(map[["ncit"]]),
    !any(is.na(map[["ncit"]])),
    !any(is.na(map[["oncotree"]]))
)
map <- map[order(map[["ncit"]]), ]
rownames(map) <- map[["ncit"]]

