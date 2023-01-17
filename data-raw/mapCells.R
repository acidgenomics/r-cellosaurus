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
ok <- !is.na(df[[rridCol]])
map[["depmap"]] <- df[ok, c(nameCol, rridCol)]
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
ok <- !is.na(df[[rridCol]])
map[["cmp"]] <- df[ok, c(nameCol, rridCol)]
map[["cmpFail"]] <- df[!ok, nameCol]
saveRDS(map, "mapCells.rds")
