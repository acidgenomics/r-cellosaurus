## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(AcidBase)
})
## nolint end
load_all()
object <- Cellosaurus()
saveRDS(object, "celloFull.rds")
object <- object[seq_len(10L), ]
cello <- object
use_data(cello, overwrite = TRUE)
## FIXME Add this to AcidBase package.
majMinVer <- numeric_version(paste(
    strsplit(
        as.character(packageVersion("Cellosaurus")),
        split = ".",
        fixed = TRUE
    )[[1L]][1L:2L],
    collapse = "."
))
shell(
    command = "aws",
    args = c(
        "--profile=acidgenomics",
        "s3", "cp", "celloFull.rds",
        pasteURL(
            "r.acidgenomics.com",
            "testdata",
            "cellosaurus",
            paste0("v", majMinVer),
            "celloFull.rds"
            protocol = "s3"
        )
    )
)
