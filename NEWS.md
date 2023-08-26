# Release notes

## Cellosaurus 0.6.0 (2023-08-23)

New functions:

- `excludeProblematicCells`: Exclude (remove) cell lines from the `Cellosaurus`
  object that are labeled as `"Problematic cell line"` in the comments. Note
  that this function is more strict than `excludeContaminatedCells`, which
  are a subset of problematic cells on Cellosaurus.
- `excludeContaminatedCells`: Exclude cell lines that are labeled as
  `"Problematic cell line: Contaminated"` in the comments.

Major changes:

- `Cellosaurus`: Return now includes OncoTree metadata, which are mapped against
  the NCI thesaurus disease identifiers.

Minor changes:

- `Cellosaurus` generator now returns `isContaminated` column, which is useful
  for differentiating between `isProblematic` lines, which may simply be
  misidentified, versus cell lines that are _really_ problematic due to
  contamination issues.
- Updated internal taxonomy parsing code to sanitize organism into full Latin
  name (e.g. "Homo sapiens") from "Homo sapiens (Human)" without the trailing
  nickname defined in the parentheses.
- Resaved example `cello` object.

## Cellosaurus 0.5.4 (2023-07-03)

New functions:

- `currentCellosaurusVersion`: Check the Cellosaurus server for current release
  version. Currently returns as `integer`.

Minor changes:

- `Cellosaurus`: Updated key for `samplingSite` metadata column, which is
  now defined as `Derived from site` in 46 release update.
- Updated Acid Genomics dependencies.

## Cellosaurus 0.5.3 (2023-05-17)

Minor changes:

- `mapCells`: Reworked our internal matching code.
- Now using new `matchNested` function internally.
- Consistently dispatching on `DFrame` instead of `DataFrame` virtual class.
- Split out internal `.processEntry` function.

## Cellosaurus 0.5.2 (2023-02-10)

Minor changes:

- Reworked DataFrame rbind step using our `rbindToDataFrame` function instead of
  data.table `rbindlist`.
- Updated dependencies to support new Bioconductor 3.17 release.

## Cellosaurus 0.5.1 (2023-01-31)

Minor changes:

- `Cellosaurus`: Fixed `ncitDiseaseId` and `ncitDiseaseName` mapping issue with
  accessions containing multiple matches (e.g. `"CVCL_0011"`, `"CVCL_0028"`).
- Switched from future.apply to parallel, for optional parallel tasks.
- Improved coverage of expected data return.

## Cellosaurus 0.5.0 (2023-01-25)

Major changes:

- `Cellosaurus`: Completely reworked main generator function. Now the package
  parses the `cellosaurus.txt` file internally instead of the previously used
  `cellosaurus.obo` file. We ran into OBO parser issues with the current
  `cellosaurus.obo` file (release 44). Also, only the `cellosaurus.txt` file
  contains additional useful metadata, including secondary accessions and the
  patient age at sampling. We have attempted to standardize metadata columns
  in the returned `Cellosaurus` object to better match the naming conventions
  currently used on the Cellosaurus website.
- `export`: Updated method to drop nested list columns (`SimpleList`) from the
  exported CSV file. Dropped columns currently include: `"comments"`,
  `"crossReferences"`, `"date"`, `"diseases"`, `"hierarchy"`,
  `"originateFromSameIndividual"`, `"referencesIdentifiers"`,
  `"strProfileData"`, `"webPages"`.
- `mapCells`: Updated mapping engine to also support secondary accession
  identifiers, which is very useful for redirected previously used identifiers
  that are still present in DepMap and Sanger CellModelPassports databases. Also
  reworked approach for handling standardized cell names at the last step, to
  avoid mapping issues with tricky cell line names, like ICC2 vs. ICC-2. These
  are non-breaking changes that are tested to map against all supported cell
  lines on DepMap and Sanger CellModelPassports.

## Cellosaurus 0.4.1 (2023-01-18)

Minor changes:

- Added cell name mapping code coverage against DepMap 22Q2, which differs
  significantly from DepMap 22Q4.
- `mapCells`: Now supports return of cell line name.
- Removed BT-549 cell line mapping override. Sanger CMP is currently incorrect.
- Added some additional identifier aliases to support DepMap 22Q2 coverage.

## Cellosaurus 0.4.0 (2023-01-18)

Major changes:

- `mapCells`: Reworked internal matching engine, and added support for manual
  overrides using `overrides` object defined in `sysdata.rda`. The original
  mappings are defined in `overrides.csv` (see `data-raw`). Mappings are now
  covered against all cell lines defined in DepMap (22Q4) and Sanger
  CellModelPassports.

Minor changes:

- `Cellosaurus`: removed option to override caching manually with `cache`.
- `sanitizeCells`: Added an additional handling rule for edge case.
- `Cellosaurus` object now gets saved with `packageVersion` in `metadata`.
- Resaved example `cello` object.

## Cellosaurus 0.3.4 (2023-01-12)

Minor changes:

- `Cellosaurus`: Fix for `"CVCL_7082"` line, which is actually named `"NA"`.
- `standardizeCells`: Fix for handling of all cells in Cellosaurus database.
- `mapCells`: Added some additional name variant rules for better matching.

## Cellosaurus 0.3.3 (2023-01-12)

Major changes:

- Now pinning `cellosaurus.obo` file internally at `r.acidgenomics.com`
  server instead of downloading the latest release version from
  `ftp.expasy.org`. This change was made due to breaking changes introduced in
  Cellosaurus 44 release that broke the package.

Minor changes:

- Improved standardization of column names (e.g. `depmapId` instead of
  `depMapId`; `sangerModelId` instead of `sangerId`), for better consistency
  with DepMapAnalysis and CellModelPassports packages.
- Added `cache` override option to main `Cellosaurus` generator, which makes
  updating to latest version (e.g. 43), more intuitive than having to delete
  the BiocFileCache directory.

## Cellosaurus 0.3.2 (2022-08-24)

Minor changes:

- `export`: Harden inheritance of S4 methods, to ensure that we class on
  `Cellosaurus`, instead of inheriting the default method for `DataFrame`.

## Cellosaurus 0.3.1 (2022-08-24)

Minor changes:

- `Cellosaurus` class now returns with `sex` metadata column.
- Factor columns are now automatically handled using `factorize` internally,
  and all applicable vectors are converted to `Rle` for improved memory
  efficiency.
- `export`: Added initial experimental method support for export of
  Cellosaurus metadata, that dynamically drops columns that aren't useful
  in CSV format.

## Cellosaurus 0.3.0 (2022-06-03)

This is a major update, with breaking changes.

New S4 classes:

- `Cellosaurus`: Now defining this class instead of `CellosaurusTable`.
  Data is retrieved using ontologyIndex from Cellosaurus FTP server instead
  of querying the website directly.

Major changes:

- `mapCells`: Now supports return of multiple identifier key types, including
  Cellosaurus (default), DepMap, and Sanger (for Cell Model Passports).
- Now using taxizedb internally for NCBI taxonomy identifier matching to full
  Latin organism name (species; e.g. "Homo sapiens").

## Cellosaurus 0.2.1 (2022-05-11)

Minor changes:

- Bug fix for breaking change in pipette namespace.

## Cellosaurus 0.2.0 (2022-04-28)

Major changes:

- Split out basejump dependencies.
- `CellosaurusTable`: Added support for return of more identifier columns.
  Improved support for handling of non-human (e.g. mouse) cell lines.
- Updated `CellosaurusTable` to use R 4.2-specific `formula` call.
- S4 class inherits from `DFrame` now, due to a breaking change introduced
  with Bioconductor 3.15, where `DataFrame` no longer works.

## Cellosaurus 0.1.1 (2021-03-12)

Minor changes:

- Updated basejump dependencies and removed unnecessary stringr import.

## Cellosaurus 0.1.0 (2021-02-21)

Minor changes:

- Reworked NAMESPACE, following basejump v0.14 release series update.
- Simplified the number of dependencies, and removed need for internal dplyr
  code, instead using new `rbindToDataFrame` approach.
- Removed internal dependency on BiocParallel, so as to not query the
  Cellosaurus server too frequently.

## Cellosaurus 0.0.3 (2020-10-08)

Major changes:

- Renamed package from cellosaurus to Cellosaurus.

Minor changes:

- Updated dependency package version requirements.

## cellosaurus 0.0.2 (2020-10-01)

Minor changes:

- Converted `mapCells` and `standardizeCells` functions to S4 methods that work
  on character class. We may define methods for these generics that work on
  classed objects inside the DepMapAnalysis package.

## cellosaurus 0.0.1 (2020-10-01)

Initial release.
