## Map NCI thesaurus (NCIt) identifiers to OncoTree codes.
## Updated 2023-08-24.
##
## NCI thesaurus
## https://ncithesaurus.nci.nih.gov/
## OncoTree
## https://oncotree.mskcc.org/

## nolint start
suppressPackageStartupMessages({
    library(goalie)
    library(S4Vectors)
    library(pipette)
    library(AcidPlyr)
})
## nolint end

map <- list()

## (1) OncoTree API ============================================================

json <- getJSON("http://oncotree.mskcc.org/api/tumorTypes")
lst <- lapply(
    X = json,
    FUN = function(x) {
        ncit <- x[["externalReferences"]][["NCI"]][[1L]]
        if (length(ncit) == 0L) {
            ncit <- NA
        }
        oncotree <- x[["code"]]
        oncotree <- rep(oncotree, length(ncit))
        data.frame("ncit" = ncit, "oncotree" = oncotree)
    }
)
df <- do.call(what = rbind, args = lst)
df <- as(df, "DFrame")
df <- df[complete.cases(df), ]
df <- df[order(df[["ncit"]]), ]
## Drop any identifiers that multi-map.
dupes <- df[["ncit"]][which(duplicated(df[["ncit"]]))]
## > print(dupes)
## [1] "C40090" "C40208" "C41247" "C4290"  "C4519"  "C6344"  "C7724"  "C8106"
## [9] "C9296"  "C9461"
df <- df[!df[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df[["ncit"]]))
rownames(df) <- NULL
map[["oncotree"]] <- df
rm(df, dupes, json, lst)

## (2) cBioPortal ==============================================================

## cBioPortal oncotree mappings file.
df <- import(
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
df <- as(df, "DFrame")
df <- df[, c("NCIT_CODE", "ONCOTREE_CODE")]
colnames(df) <- c("ncit", "oncotree")
## Need to sanitize "C7356��","GBASC".
df[which(df[["ncit"]] == "C7356��"), "ncit"] <- "C7356"
df <- df[complete.cases(df), ]
df <- unique(df)
## e.g. "C9168,C7967" for "SRCCR".
df[["ncit"]] <- strsplit(df[["ncit"]], split = ",")
df <- unnest2(df, col = "ncit")
df <- unique(df)
df <- df[order(df[["ncit"]]), ]
## Drop any identifiers that multi-map.
dupes <- df[["ncit"]][which(duplicated(df[["ncit"]]))]
## > print(dupes)
## [1] "C27349" "C27893" "C3058"  "C3768"  "C40090" "C40208" "C40364" "C41247"
## [9] "C4221"  "C4290"  "C4519"  "C4906"  "C6344"  "C7151"  "C7724"  "C8106"
## [17] "C82616" "C9245"  "C9296"  "C9461"
df <- df[!df[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df[["ncit"]]))
rownames(df) <- NULL
map[["cbioportal"]] <- df
rm(df, dupes)

## (3) DepMap ==================================================================

## Can't use the DepMap "Model.csv" file directly currently, as it doesn't
## contain any NCIt identifier mappings.

df <- DepMapAnalysis:::.importBroadModelInfo() # nolint
df <- DataFrame(
    "ncit" = df[["cellosaurus"]][["ncitDiseaseId"]],
    "oncotree" = df[["broad"]][["OncotreeCode"]]
)
df <- df[!is.na(df[["oncotree"]]), ]
df <- unique(df)
df <- unnest2(df, col = "ncit")
df <- unique(df)
df <- df[order(df[["ncit"]]), ]
## Drop any identifiers that multi-map.
dupes <- df[["ncit"]][which(duplicated(df[["ncit"]]))]
## [1] "C105555" "C129857" "C132105" "C27677"  "C27769"  "C2923"   "C2926"
## [8] "C2993"   "C3163"   "C3224"   "C3224"   "C3224"   "C3247"   "C3273"
## [15] "C34447"  "C3467"   "C3493"   "C3493"   "C3510"   "C3512"   "C3512"
## [22] "C36080"  "C3716"   "C4001"   "C4004"   "C4017"   "C4017"   "C4025"
## [29] "C4043"   "C4194"   "C4194"   "C4194"   "C4194"   "C4349"   "C4436"
## [36] "C4436"   "C4450"   "C45544"  "C45663"  "C4872"   "C4872"   "C4872"
## [43] "C4908"   "C4912"   "C5214"   "C5228"   "C5250"   "C5473"   "C60781"
## [50] "C7320"   "C7542"   "C7558"   "C8263"   "C9039"   "C9142"   "C9143"
## [57] "C9154"   "C9383"   "C9385"   "C9477"
df <- df[!df[["ncit"]] %in% dupes, ]
assert(hasNoDuplicates(df[["ncit"]]))
rownames(df) <- NULL
map[["depmap"]] <- df
rm(df, dupes)

## Combine mappings ============================================================

df <- do.call(what = rbind, args = map)
df <- as(df, "DFrame")
df <- df[!duplicated(df[["ncit"]]), ]
assert(
    hasNoDuplicates(df[["ncit"]]),
    !anyNA(df[["ncit"]]),
    !anyNA(df[["oncotree"]])
)
df <- df[order(df[["ncit"]]), ]
rownames(df) <- NULL
assert(
    allAreMatchingRegex(x = df[["ncit"]], pattern = "^C[0-9]+$"),
    allAreMatchingRegex(x = df[["oncotree"]], pattern = "^[-/_A-Z0-9]+$")
)
saveRDS(df, "ncit2Oncotree.rds")
export(df, "ncit2Oncotree.csv")
