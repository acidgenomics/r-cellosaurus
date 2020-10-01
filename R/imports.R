#' @importMethodsFrom S4Vectors coerce
#' @importFrom BiocParallel bplapply bpparam
#' @importFrom cli cli_alert_warning
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange_all group_by summarize ungroup
#' @importFrom goalie allAreMatchingFixed assert hasLength hasRownames hasRows
#'   isCharacter isString isSubset validate
#' @importFrom httr GET content
#' @importFrom methods as new setClass setValidity
#' @importFrom pipette import
#' @importFrom rlang !! sym
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect str_match str_replace str_replace_all
#'   str_split str_subset
#' @importFrom syntactic camelCase snakeCase
#' @importFrom tibble column_to_rownames tibble
#' @importFrom utils URLencode head
NULL
