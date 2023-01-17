.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' Cellosaurus release version
#'
#' @note Updated 2023-01-12.
#' @noRd
#'
#' @details
#' Note that ontologyIndex currently has parsing problems with 44 release.
.release <- 43L



#' pipette test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-03-16.
#'
#' @examples
#' pipetteTestsURL
.testsURL <- paste0(
    "https://r.acidgenomics.com/testdata/", tolower(.pkgName), "/",
    "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
)
