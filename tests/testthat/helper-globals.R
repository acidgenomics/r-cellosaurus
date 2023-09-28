utils::data(cello, envir = environment())

## nolint start
hasRows <- goalie::hasRows
with_collate <- withr::with_collate
## nolint end

celloFull <- readRDS(file.path(cacheDir, "celloFull.rds"))
map <- readRDS(file.path(cacheDir, "mapCells.rds"))
