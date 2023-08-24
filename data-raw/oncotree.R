## OncoTree metadata from DepMap.
## Updated 2023-08-24.

library(syntactic)
library(pipette)

## DepMap 23q2 "Model.csv" file.
df <- import(
    con = pasteURL(
        "figshare.com",
        "ndownloader",
        "files",
        "40448834",
        protocol = "https"
    ),
    format = "csv"
)
colnames(df) <- camelCase(colnames(df))
df <- df[
    ,
    c(
        "oncotreeCode",
        "oncotreeSubtype",
        "oncotreePrimaryDisease",
        "oncotreeLineage"
    )
]
df <- df[complete.cases(df), ]
df <- df[order(df[["oncotreeCode"]]), ]
df <- unique(df)
export(df, "oncotree.csv")
