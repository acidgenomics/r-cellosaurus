## Updated 2021-02-21.
.strSubsetAndMatch <- function(string, pattern) {
    string <- str_subset(string = string, pattern = pattern)
    match <- str_match(string = string, pattern = pattern)
    match
}



## Updated 2021-02-21.
.strSubsetAndMatchSingle <- function(string, pattern) {
    match <- .strSubsetAndMatch(string = string, pattern = pattern)
    if (hasRows(match)) {
        out <- match[1L, 2L]
    } else {
        out <- NA
    }
    out
}
