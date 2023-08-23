## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(syntactic)
    library(pipette)
})
## nolint end
load_all()

## datasets ====================================================================

## Datasets JSON from "datasets.json".
datasets <- import(
    con = system.file(
        "extdata",
        "datasets.json",
        package = .pkgName,
        mustWork = TRUE
    )
)

## ontologyMappings ============================================================

## FIXME Just save this in Cellosaurus and reference in DepMapAnalysis instead.
## Consider renaming this to oncotreeMappings instead.

## OncoTree ontology mappings, which map to NCIt.
## @seealso
## - https://github.com/cBioPortal/oncotree/tree/master/scripts/
##     ontology_to_ontology_mapping_tool

ontologyMappings <- import(
    con = system.file(
        "extdata",
        "ontologyMappings.tsv",
        package = .pkgName,
        mustWork = TRUE
    )
)
colnames(ontologyMappings) <- camelCase(colnames(ontologyMappings))
## Arrange by "oncotreeCode".
ontologyMappings <-
    ontologyMappings[order(ontologyMappings[["oncotreeCode"]]), , drop = FALSE]

## Save `R/sysdata.rda` ========================================================
use_data(
    datasets,
    ontologyMappings,
    overwrite = TRUE,
    internal = TRUE
)
