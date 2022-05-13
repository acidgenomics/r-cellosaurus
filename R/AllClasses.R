#' Cellosaurus table of cell identifier mappings
#'
#' @note Updated 2022-05-13.
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
                "ancestors" = "character",
                "comment" = "CompressedCharacterList",
                "creationDate" = "character",
                "depMapId" = "character",
                "derivedFrom" = "character",
                "id" = "character",
                "isCancer" = "logical",
                "isProblematic" = "logical",
                "isSymmetric" = "character",
                "isTransitive" = "character",
                "name" = "character",
                "ncbiTaxonomyId" = "Rle",
                "ncitDiseaseId" = "Rle",
                "ncitDiseaseName" = "Rle",
                "obsolete" = "logical",
                "organism" = "Rle",
                "originateFromSameIndividualAs" = "CompressedCharacterList",
                "sangerId" = "character",
                "subset" = "CompressedCharacterList",
                "synonym" = "CompressedCharacterList",
                "xref" = "CompressedCharacterList"
            ),
            subset = FALSE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)
