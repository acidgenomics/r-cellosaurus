.str_subset_and_match <- function(string, pattern) {
    string <- str_subset(string = string, pattern = pattern)
    match <- str_match(string = string, pattern = pattern)
    match
}
