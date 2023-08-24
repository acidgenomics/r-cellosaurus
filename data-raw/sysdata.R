## Save `R/sysdata.rda` file.
## Updated 2023-08-24.

## nolint start
library(usethis)
## nolint end

overrides <- import("overrides.csv")

use_data(
    overrides,
    overwrite = TRUE,
    internal = TRUE
)
