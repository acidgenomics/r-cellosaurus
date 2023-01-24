if (isFALSE(goalie::hasInternet())) {
    warning("No Internet connection detected.")
    return(invisible(NULL))
}
dir.create("cache", showWarnings = FALSE)
files <- c(
    "celloFull.rds",
    "mapCells.rds"
)
invisible(Map(
    f = function(remoteDir, file, envir) {
        destfile <- file.path("cache", file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        "envir" = environment(),
        "remoteDir" = .testsUrl
    )
))
rm(files)
