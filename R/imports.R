## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame DataFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase encode factorize leftJoin mapCells
#' removeNA sanitizeNA snakeCase standardizeCells
#' @importFrom BiocGenerics %in% as.data.frame grep grepl lapply unlist
#' @importFrom IRanges gsub
#' @importFrom S4Vectors Rle decode metadata metadata<-
#' @importFrom methods show
#' @importFrom pipette export import
#'
#' @importMethodsFrom AcidPlyr leftJoin
#' @importMethodsFrom pipette encode export factorize import removeNA sanitizeNA
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL showHeader showSlotInfo
#' @importFrom AcidCLI abort alert
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreMatchingFixed allAreMatchingRegex assert hasLength
#' hasNoDuplicates hasRownames isCharacter isFlag isString isSubset validate
#' validateClasses
#' @importFrom methods as is new setClass setMethod setValidity validObject
#' @importFrom pipette cacheURL
#' @importFrom stringi stri_replace_all_regex stri_replace_first_regex
#' @importFrom taxizedb db_download_ncbi taxid2name
#' @importFrom utils packageName
NULL
