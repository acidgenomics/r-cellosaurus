# Cellosaurus

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
    ),
    dependencies = TRUE
)
```

### [Docker][] method

```sh
image='acidgenomics/r-packages:cellosaurus'
workdir='/mnt/work'
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[cellosaurus]: https://web.expasy.org/cellosaurus/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
