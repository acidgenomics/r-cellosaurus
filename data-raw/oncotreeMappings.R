## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(syntactic)
    library(pipette)
})
## nolint end
load_all()

## NCIt to OncoTree mappings ===================================================

## NCI thesaurus
##   https://ncithesaurus.nci.nih.gov/
## OncoTree
##   https://oncotree.mskcc.org/

## cBioPortal oncotree mappings file.
url <- "https://raw.githubusercontent.com/cBioPortal/oncotree/master/scripts/ontology_to_ontology_mapping_tool/ontology_mappings.txt"


## DepMap 23q2 "Model.csv" file
## https://figshare.com/ndownloader/files/40448834


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
