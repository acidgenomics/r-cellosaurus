#' Cellosaurus release version
#'
#' @note Updated 2023-01-20.
#' @noRd
#'
#' @details
#' Note that ontologyIndex currently has parsing problems with 44 release.
.release <- 44L



.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



.testsURL <- paste0(
    "https://r.acidgenomics.com/testdata/", tolower(.pkgName), "/",
    "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
)
