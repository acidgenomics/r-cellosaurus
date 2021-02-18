#' Cellosaurus
#'
#' Cellosaurus identifier mapping toolkit.
#'
#' @keywords internal
#'
#' @importClassesFrom basejump DataFrame
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom basejump alertWarning camelCase complete.cases head import
#'   rbindlist snakeCase
#' @importFrom goalie allAreMatchingFixed assert hasLength hasRownames hasRows
#'   isCharacter isString isSubset validate
#' @importFrom httr GET content
#' @importFrom methods as new setClass setMethod setValidity
#' @importFrom stringr str_detect str_match str_replace str_replace_all
#'   str_split str_subset
"_PACKAGE"



## FIXME Can we rethink this?
#' @importFrom BiocParallel bplapply
#' @importFrom dplyr arrange_all group_by summarize ungroup
#' @importFrom rlang !! sym
#' @importFrom tibble column_to_rownames
#' @importFrom utils URLencode
NULL
