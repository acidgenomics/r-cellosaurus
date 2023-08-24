## Map NCI thesaurus (NCIt) identifiers to OncoTree codes.
## Updated 2023-08-24.
##
## NCI thesaurus
##   https://ncithesaurus.nci.nih.gov/
## OncoTree
##   https://oncotree.mskcc.org/

## nolint start
suppressPackageStartupMessages({
    library(goalie)
    library(S4Vectors)
    library(pipette)
    library(AcidPlyr)
})
## nolint end

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
df <- df[complete.cases(df), ]
df <- df[order(df[["ncit"]]), ]
rownames(df) <- NULL
saveRDS(df, "ncit2Oncotree.rds")
export(df, "ncit2Oncotree.csv")
