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
map[["cmp"]] <- df
saveRDS(map, "mapCells.rds")
