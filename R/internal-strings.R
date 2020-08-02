.str_subset_and_match <- function(string, pattern) {
    string <- str_subset(string = string, pattern = pattern)
    match <- str_match(string = string, pattern = pattern)
    match
}

.str_subset_and_match_single <- function(string, pattern) {
    match <- .str_subset_and_match(string = string, pattern = pattern)
    if (hasRows(match)) {
        out <- match[1L, 2L]
    } else {
        out <- NULL
    }
    out
}
