utils::data(cello, envir = environment())

## nolint start
hasRows <- goalie::hasRows
## nolint end

celloFull <- readRDS(file.path("cache", "celloFull.rds"))
map <- readRDS(file.path("cache", "mapCells.rds"))
