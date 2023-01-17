## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
## nolint end
load_all()
object <- Cellosaurus()
saveRDS(object, "celloFull.rds")
object <- object[seq_len(10L), ]
cello <- object
use_data(cello, overwrite = TRUE)
