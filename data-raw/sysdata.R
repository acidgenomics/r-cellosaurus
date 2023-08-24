## Save `R/sysdata.rda` file.
## Updated 2023-08-24.

## nolint start
suppressPackageStartupMessages({
    library(usethis)
})
## nolint end

ncit2Oncotree <- readRDS("ncit2Oncotree.rds")
oncotree <- readRDS("oncotree.rds")
overrides <- readRDS("overrides.rds")

use_data(
    ncit2Oncotree,
    oncotree,
    overrides,
    overwrite = TRUE,
    internal = TRUE
)
