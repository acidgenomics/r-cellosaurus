#' Filter problematic and/or contaminated cell lines
#'
#' @name filterProblematicCells
#' @note Updated 2023-08-23.
#'
#' @details
#' `filterProblematicCells` filters based on `"Problematic cell line"` metadata
#' in cell line comments. This also filters contaminated cell lines, and is
#' more strict.
#'
#' `filterContaminatedCells` only filters based on
#' `"Problematic cell line: Contaminated"` metadata in cell line comments. This
#' is less stringent than `filterProblematicCells` and intentionaly keeps
#' cell lines that are misidentified but not contaminated.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Modified object, with cell lines removed.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' x <- filterProblematicCells(object)
#' print(x)
#' y <- filterContaminatedCells(object)
#' print(y)
NULL


