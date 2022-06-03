# Cellosaurus 0.3.0 (2022-06-03)

This is a major update, with breaking changes.

## New S4 classes

- `Cellosaurus`: Now defining this class instead of `CellosaurusTable`.
  Data is retrieved using ontologyIndex from Cellosaurus FTP server instead
  of querying the website directly.

## Major changes

- `mapCells`: Now supports return of multiple identifier key types, including
  Cellosaurus (default), DepMap, and Sanger (for Cell Model Passports).
- Now using taxizedb internally for NCBI taxonomy identifier matching to full
  Latin organism name (species; e.g. "Homo sapiens").

# Cellosaurus 0.2.1 (2022-05-11)

## Minor changes

- Bug fix for breaking change in pipette namespace.

# Cellosaurus 0.2.0 (2022-04-28)

## Major changes

- Split out basejump dependencies.
- `CellosaurusTable`: Added support for return of more identifier columns.
  Improved support for handling of non-human (e.g. mouse) cell lines.
- Updated `CellosaurusTable` to use R 4.2-specific `formula` call.
- S4 class inherits from `DFrame` now, due to a breaking change introduced
  with Bioconductor 3.15, where `DataFrame` no longer works.

# Cellosaurus 0.1.1 (2021-03-12)

## Minor changes

- Updated basejump dependencies and removed unnecessary stringr import.

# Cellosaurus 0.1.0 (2021-02-21)

## Minor changes

- Reworked NAMESPACE, following basejump v0.14 release series update.
- Simplified the number of dependencies, and removed need for internal dplyr
  code, instead using new `rbindToDataFrame` approach.
- Removed internal dependency on BiocParallel, so as to not query the
  Cellosaurus server too frequently.

# Cellosaurus 0.0.3 (2020-10-08)

## Major changes

- Renamed package from cellosaurus to Cellosaurus.

## Minor changes

- Updated dependency package version requirements.

# cellosaurus 0.0.2 (2020-10-01)

## Minor changes

- Converted `mapCells` and `standardizeCells` functions to S4 methods that work
  on character class. We may define methods for these generics that work on
  classed objects inside the DepMapAnalysis package.

# cellosaurus 0.0.1 (2020-10-01)

Initial release.
