## Prepare minimal `cello` object used in working examples.
## Updated 2023-08-24.

## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(AcidBase)
})
## nolint end

load_all(helpers = FALSE)
object <- Cellosaurus()
saveRDS(object, "celloFull.rds")
object <- object[seq_len(10L), ]
cello <- object
use_data(cello, overwrite = TRUE)
shell(
    command = "aws",
    args = c(
        "--profile=acidgenomics",
        "s3", "cp", "celloFull.rds",
        pasteURL(
            "r.acidgenomics.com",
            "testdata",
            tolower(.pkgName),
            paste0("v", majorMinorVersion(.pkgVersion)),
            "celloFull.rds",
            protocol = "s3"
        )
    )
)
unlink("celloFull.rds")
