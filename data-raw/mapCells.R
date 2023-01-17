suppressPackageStartupMessages({
    library(AcidBase)
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
nameCol <- "CellLineName"
rridCol <- "RRID"
df <- df[!is.na(df[[nameCol]]), c(nameCol, rridCol)]
ok <- !is.na(df[[rridCol]])
map[["depmap"]] <- df[ok, ]
map[["depmapFail"]] <- df[!ok, nameCol]
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
nameCol <- "model_name"
rridCol <- "RRID"
df <- df[!is.na(df[[nameCol]]), c(nameCol, rridCol)]
ok <- !is.na(df[[rridCol]])
map[["cmp"]] <- df[ok, ]
map[["cmpFail"]] <- df[!ok, nameCol]
saveRDS(map, "mapCells.rds")
