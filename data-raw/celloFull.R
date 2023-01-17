## nolint start
suppressPackageStartupMessages({
    library(devtools)
})
## nolint end
load_all()
object <- Cellosaurus()
saveRDS(object, "celloFull.rds")
