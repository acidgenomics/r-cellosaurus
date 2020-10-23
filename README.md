# Cellosaurus

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/cellosaurus.svg?branch=master)](https://travis-ci.com/acidgenomics/cellosaurus)

[Cellosaurus][] identifier mapping toolkit.

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "Cellosaurus",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[cellosaurus]: https://web.expasy.org/cellosaurus/
[r]: https://www.r-project.org/
