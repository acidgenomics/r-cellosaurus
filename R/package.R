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

#' @importFrom AcidGenerics camelCase cellsPerGeneFusion cellsPerMutation encode
#' excludeContaminatedCells excludeNonCancerCells excludeNonHumanCells
#' excludeProblematicCells export geneFusions import leftJoin mapCells
#' matchNested mutations rbindToDataFrame selectCells snakeCase standardizeCells
#' tnbc
#' @importFrom BiocGenerics Map %in% as.data.frame cbind do.call grep grepl
#' lapply rbind table unique unlist
#' @importFrom IRanges gsub sub
#' @importFrom S4Vectors %in% Rle decode metadata metadata<- na.omit
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase matchNested
#' @importMethodsFrom AcidPlyr leftJoin rbindToDataFrame
#' @importMethodsFrom pipette as.DataFrame encode export import removeNa
#' sanitizeNa
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase majorVersion pasteUrl showHeader showSlotInfo strSplit
#' @importFrom AcidCLI abort alert alertInfo
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom goalie allAreMatchingFixed allAreMatchingRegex areSetEqual assert
#' bapply hasLength hasNames hasNoDuplicates hasRownames isCharacter isFlag
#' isInt isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setMethod setValidity validObject
#' @importFrom parallel mclapply
#' @importFrom pipette cacheUrl
#' @importFrom utils packageName packageVersion
NULL
