suppressPackageStartupMessages({
    library(AcidBase)
    library(syntactic)
    library(pipette)
})
map <- list()
## DepMap ======================================================================
## 22Q4 release "Model.csv"
df <- import(
    con = cacheURL(pasteURL(
        "ndownloader.figshare.com",
        "files",
        "38466923",
        protocol = "https"
    )),
    format = "csv"
)
df <- df[, c("ModelID", "CellLineName", "RRID")]
rownames(df) <- makeNames(df[["ModelID"]])
df <- df[!is.na(df[["CellLineName"]]), ]
df <- df[!duplicated(df[["CellLineName"]]), ]
map[["depmapFail"]] <- df[is.na(df[["RRID"]]), "CellLineName"]
df <- df[!is.na(df[["RRID"]]), ]
## Remove cell lines that map ambiguously.
censor <- c(
    "ACH_000058", # ML-1
    "ACH_001347", # H157
    "ACH_001743", # RC2
    "ACH_002099", # CS1
    "ACH_002147", # K2
    "ACH_002185", # PL18
    "ACH_002325", # LY2
    "ACH_002475" # HAP1
)
df <- df[setdiff(rownames(df), censor), ]
map[["depmap"]] <- df
## Sanger CellModelPassports ===================================================
df <- import(
    con = cacheURL(pasteURL(
        "cog.sanger.ac.uk",
        "cmp",
        "download",
        "model_list_20230110.csv",
        protocol = "https"
    )),
    format = "csv",
    engine = "readr"
)
df <- df[, c("model_id", "model_name", "RRID")]
rownames(df) <- makeNames(df[["model_id"]])
df <- df[!is.na(df[["model_name"]]), ]
df <- df[!duplicated(df[["model_name"]]), ]
map[["cmpFail"]] <- df[is.na(df[["RRID"]]), "model_name"]
df <- df[!is.na(df[["RRID"]]), ]
## Remove cell lines that map ambiguously.
censor <- c(
    "SIDM00366", # SBC-2
    "SIDM00400" # SC-1
    "SIDM00054" # PL18
    "SIDM00122" # BT-549
    "SIDM00199" # CS1
    "SIDM00408" # MS-1
    "SIDM00427" # RH-1
    "SIDM00911" # COLO-699
)
df <- df[setdiff(rownames(df), censor), ]
map[["cmp"]] <- df
saveRDS(map, "mapCells.rds")
