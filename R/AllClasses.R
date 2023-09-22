#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2023-09-22.
#' @export
#'
#' @return `Cellosaurus`.
setClass(
    Class = "Cellosaurus",
    contains = "DFrame"
)
setValidity(
    Class = "Cellosaurus",
    method = function(object) {
        ok <- validate(
            hasRownames(object),
            allAreMatchingFixed(x = rownames(object), pattern = "CVCL_")
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = object,
            expected = list(
                "accession" = "Rle",
                "ageAtSampling" = "Rle",
                "category" = "Rle",
                "cellLineName" = "Rle",
                "comments" = "SimpleList",
                "crossReferences" = "SimpleList",
                "date" = "CompressedCharacterList",
                "depmapId" = "Rle",
                "diseases" = "SimpleList",
                "hierarchy" = "CompressedCharacterList",
                "isCancer" = "Rle",
                "isContaminated" = "Rle",
                "isProblematic" = "Rle",
                "msiStatus" = "CompressedCharacterList",
                "ncbiTaxonomyId" = "CompressedIntegerList",
                "ncitDiseaseId" = "CompressedCharacterList",
                "ncitDiseaseName" = "CompressedCharacterList",
                "oncotreeCode" = "Rle",
                "oncotreeLevel" = "Rle",
                "oncotreeMainType" = "Rle",
                "oncotreeName" = "Rle",
                "oncotreeParent" = "Rle",
                "oncotreeTissue" = "Rle",
                "organism" = "CompressedCharacterList",
                "originateFromSameIndividual" = "CompressedCharacterList",
                "population" = "CompressedCharacterList",
                "referencesIdentifiers" = "SimpleList",
                "samplingSite" = "CompressedCharacterList",
                "sangerModelId" = "Rle",
                "secondaryAccession" = "CompressedCharacterList",
                "sexOfCell" = "Rle",
                "strProfileData" = "SimpleList",
                "synonyms" = "CompressedCharacterList",
                "webPages" = "CompressedCharacterList"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validate(
            hasNoDuplicates(object[["accession"]]),
            hasNoDuplicates(object[["cellLineName"]])
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)
