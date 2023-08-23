## nolint start
suppressPackageStartupMessages({
    library(pipette)
    library(usethis)
})
## nolint end
overrides <- import("overrides.csv")
## Save `R/sysdata.rda` ========================================================
use_data(
    overrides,
    overwrite = TRUE,
    internal = TRUE
)
