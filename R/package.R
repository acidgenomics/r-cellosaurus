#' Cellosaurus
#'
#' Cellosaurus identifier mapping toolkit.
#'
#' @keywords internal
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom basejump camelCase complete.cases head import snakeCase
#' @importFrom goalie allAreMatchingFixed assert hasLength hasRownames hasRows
#'   isCharacter isString isSubset validate
#' @importFrom httr GET content
#' @importFrom methods as new setClass setMethod setValidity
#' @importFrom stringr str_detect str_match str_replace str_replace_all
#'   str_split str_subset
"_PACKAGE"



## FIXME Can we rethink this?
#' @importFrom BiocParallel bplapply bpparam
#' @importFrom cli cli_alert_warning
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange_all group_by summarize ungroup
#' @importFrom rlang !! sym
#' @importFrom tibble column_to_rownames tibble
#' @importFrom utils URLencode
NULL
