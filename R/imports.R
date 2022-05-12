## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame DataFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase rbindToDataFrame snakeCase
#' @importFrom BiocGenerics grep grepl
#' @importFrom S4Vectors aggregate append complete.cases head metadata
#' metadata<-
#' @importFrom pipette import
#'
#' @importMethodsFrom AcidPlyr rbindToDataFrame
#' @importMethodsFrom pipette import
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## S3 generics =================================================================
#' @importFrom stats formula
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL
#' @importFrom AcidCLI alertWarning
#' @importFrom S4Vectors DataFrame
#' @importFrom goalie allAreMatchingFixed assert hasLength hasRownames hasRows
#' isCharacter isFlag isInt isOrganism isString isSubset validate
#' @importFrom httr GET content
#' @importFrom methods as is new setClass setMethod setValidity
#' @importFrom ontologyIndex get_ontology
#' @importFrom utils URLencode packageName
NULL


## FIXME Rework these using stringi.

#' @importFrom stringr str_detect str_match str_replace str_replace_all
#' str_split str_subset
NULL
