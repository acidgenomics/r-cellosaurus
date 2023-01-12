#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2022-09-13.
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
            allAreMatchingFixed(
                x = rownames(object),
                pattern = "CVCL_"
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = object,
            expected = list(
                "comment" = "CompressedCharacterList",
                "creationDate" = "Rle",
                "depmapId" = "Rle",
                "derivedFrom" = "Rle",
                "ethnicity" = "CompressedCharacterList",
                "id" = "Rle",
                "isCancer" = "Rle",
                "isProblematic" = "Rle",
                "msiStatus" = "Rle",
                "name" = "Rle",
                "ncbiTaxonomyId" = "Rle",
                "ncitDiseaseId" = "Rle",
                "ncitDiseaseName" = "Rle",
                "obsolete" = "Rle",
                "organism" = "Rle",
                "originateFromSameIndividualAs" = "CompressedCharacterList",
                "samplingSite" = "Rle",
                "sangerModelId" = "Rle",
                "sex" = "Rle",
                "subset" = "CompressedCharacterList",
                "synonym" = "CompressedCharacterList",
                "xref" = "CompressedCharacterList"
            ),
            subset = FALSE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validate(hasNoDuplicates(object[["id"]]))
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)
