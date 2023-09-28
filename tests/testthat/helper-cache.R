lst <- AcidDevTools::cacheTestFiles(
    pkg = .pkgName,
    files = c(
        "celloFull.rds",
        "mapCells.rds"
    )
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
