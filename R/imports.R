## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame DataFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase leftJoin removeNA sanitizeNA snakeCase
#' @importFrom BiocGenerics as.data.frame grep grepl unlist
#' @importFrom IRanges gsub
#' @importFrom S4Vectors Rle metadata metadata<-
#' @importFrom methods show
#' @importFrom pipette import
#'
#' @importMethodsFrom AcidPlyr leftJoin
#' @importMethodsFrom pipette import removeNA sanitizeNA
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL showHeader showSlotInfo
#' @importFrom AcidCLI abort
#' @importFrom IRanges CharacterList
#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreMatchingFixed assert hasLength hasNoDuplicates
#' hasRownames isCharacter isFlag isString validate validateClasses
#' @importFrom methods as is new setClass setMethod setValidity validObject
#' @importFrom pipette cacheURL
#' @importFrom stringi stri_replace_all_regex stri_replace_first_regex
#' @importFrom utils packageName
NULL
