# cellosaurus

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/cellosaurus.svg?branch=master)](https://travis-ci.com/acidgenomics/cellosaurus)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/79vm352mfa28tkwi?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/cellosaurus)

Cellosaurus identifier mapping toolkit.

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "cellosaurus",
    repos = c(
        "r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[r]: https://www.r-project.org/
