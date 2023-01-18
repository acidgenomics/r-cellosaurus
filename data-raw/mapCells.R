## nolint start
suppressPackageStartupMessages({
    library(AcidBase)
    library(syntactic)
    library(pipette)
})
## nolint end
map <- list()
## DepMap 22Q2 =================================================================
## Import "sample_info.csv" file.
df <- import(
    con = cacheURL(pasteURL(
        "ndownloader.figshare.com",
        "files",
        "35020903",
        protocol = "https"
    )),
    format = "csv",
    engine = "readr"
)
df <- df[, c("DepMap_ID", "cell_line_name", "RRID")]
rownames(df) <- makeNames(df[["DepMap_ID"]])
df <- df[!is.na(df[["cell_line_name"]]), ]
df <- df[!duplicated(df[["cell_line_name"]]), ]
map[["depmap_22q2_fail"]] <- df[is.na(df[["RRID"]]), "cell_line_name"]
df <- df[!is.na(df[["RRID"]]), ]
map[["depmap_22q2"]] <- df
## DepMap 22Q4 =================================================================
## Import "Model.csv" file
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
map[["depmap_22q4_fail"]] <- df[is.na(df[["RRID"]]), "CellLineName"]
df <- df[!is.na(df[["RRID"]]), ]
map[["depmap_22q4"]] <- df
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
map[["cmp_fail"]] <- df[is.na(df[["RRID"]]), "model_name"]
df <- df[!is.na(df[["RRID"]]), ]
map[["cmp"]] <- df
saveRDS(map, "mapCells.rds")
