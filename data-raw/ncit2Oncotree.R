## Map NCI thesaurus (NCIt) identifiers to OncoTree codes.
## Updated 2023-08-24.

## nolint start
suppressPackageStartupMessages({
    library(usethis)
    library(goalie)
    library(S4Vectors)
    library(pipette)
    library(AcidPlyr) # 0.4.1
    library(DepMapAnalysis) # 0.3.0.9000
})
## nolint end

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
## Need to sanitize "C7356��","GBASC".
df1[which(df1[["ncit"]] == "C7356��"), "ncit"] <- "C7356"
df1 <- df1[complete.cases(df1), ]
df1 <- unique(df1)
## e.g. "C9168,C7967" for "SRCCR".
df1[["ncit"]] <- strsplit(df1[["ncit"]], split = ",")
df1 <- unnest2(df1, col = "ncit")
df1 <- unique(df1)
df1 <- df1[order(df1[["ncit"]]), ]
## Drop any identifiers that multi-map.
dupes <- df1[["ncit"]][which(duplicated(df1[["ncit"]]))]
## > print(dupes)
##  [1] "C27349" "C27893" "C3058"  "C3768"  "C40090" "C40208" "C40364" "C41247"
##  [9] "C4221"  "C4290"  "C4519"  "C4906"  "C6344"  "C7151"  "C7724"  "C8106"
## [17] "C82616" "C9245"  "C9296"  "C9461"
df1 <- df1[!df1[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df1[["ncit"]]))
df1[["source"]] <- "cbioportal"

## Can't use the DepMap "Model.csv" file directly currently, as it doesn't
## contain any NCIt identifier mappings.
df2 <- DepMapAnalysis:::.importBroadModelInfo()
df2 <- DataFrame(
    "ncit" = df2[["cellosaurus"]][["ncitDiseaseId"]],
    "oncotree" = df2[["broad"]][["OncotreeCode"]]
)
df2 <- df2[!is.na(df2[["oncotree"]]), ]
df2 <- unique(df2)
df2 <- unnest2(df2, col = "ncit")
df2 <- unique(df2)
df2 <- df2[order(df2[["ncit"]]), ]
## Drop any identifiers that multi-map.
dupes <- df2[["ncit"]][which(duplicated(df2[["ncit"]]))]
##  [1] "C105555" "C129857" "C132105" "C27677"  "C27769"  "C2923"   "C2926"
##  [8] "C2993"   "C3163"   "C3224"   "C3224"   "C3224"   "C3247"   "C3273"
## [15] "C34447"  "C3467"   "C3493"   "C3493"   "C3510"   "C3512"   "C3512"
## [22] "C36080"  "C3716"   "C4001"   "C4004"   "C4017"   "C4017"   "C4025"
## [29] "C4043"   "C4194"   "C4194"   "C4194"   "C4194"   "C4349"   "C4436"
## [36] "C4436"   "C4450"   "C45544"  "C45663"  "C4872"   "C4872"   "C4872"
## [43] "C4908"   "C4912"   "C5214"   "C5228"   "C5250"   "C5473"   "C60781"
## [50] "C7320"   "C7542"   "C7558"   "C8263"   "C9039"   "C9142"   "C9143"
## [57] "C9154"   "C9383"   "C9385"   "C9477"
df2 <- df2[!df2[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df2[["ncit"]]))
df2[["source"]] <- "cbioportal"

## Combine the mappings from cBioPortal (df1) and DepMap (df2).
map <- rbind(df1, df2)
map <- map[, c("ncit", "oncotree")]
map <- unique(map)
## Drop any identifiers that multi-map.
dupes <- map[["ncit"]][which(duplicated(map[["ncit"]]))]
## [1] "C3171"  "C3208"  "C3752"  "C3844"  "C4029"  "C45716" "C80334"
map <- map[!map[["ncit"]] %in% dupes, ]
assert(
    hasNoDuplicates(map[["ncit"]]),
    !any(is.na(map[["ncit"]])),
    !any(is.na(map[["oncotree"]]))
)
map <- map[order(map[["ncit"]]), ]
rownames(map) <- NULL
assert(
    allAreMatchingRegex(x = map[["ncit"]], pattern = "^C[0-9]+$"),
    allAreMatchingRegex(x = map[["oncotree"]], pattern = "^[-/_A-Z0-9]+$")
)
export(map, "ncit2Oncotree.csv")
