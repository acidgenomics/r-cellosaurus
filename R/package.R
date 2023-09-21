#' Cellosaurus
#'
#' Cellosaurus identifier mapping toolkit.
#'
#' @aliases NULL
#' @keywords internal
"_PACKAGE"



## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase encode excludeContaminatedCells
#' excludeProblematicCells export import leftJoin mapCells matchNested
#' rbindToDataFrame selectCells snakeCase standardizeCells tnbc
#' @importFrom BiocGenerics Map %in% as.data.frame grep grepl lapply unique
#' unlist
#' @importFrom IRanges gsub sub
#' @importFrom S4Vectors Rle decode metadata metadata<- na.omit
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase matchNested
#' @importMethodsFrom AcidPlyr leftJoin rbindToDataFrame
#' @importMethodsFrom pipette encode export import removeNA sanitizeNA
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase majorVersion pasteURL showHeader showSlotInfo strSplit
#' @importFrom AcidCLI abort alert alertInfo
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom goalie allAreMatchingFixed allAreMatchingRegex areSetEqual assert
#' bapply hasLength hasNames hasNoDuplicates hasRownames isCharacter isFlag
#' isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setMethod setValidity validObject
#' @importFrom parallel mclapply
#' @importFrom pipette cacheURL
#' @importFrom utils packageName packageVersion
NULL
