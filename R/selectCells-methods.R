## FIXME Allow user to select isCancer, isContaminated, and isProblematic.



#' @name selectCells
#' @inherit AcidGenerics::selectCells
#' @note Updated 2023-09-12.
#'
#' @details
#' Only exact matching is currently supported.
#'
#' @section Supported keys:
#'
#' - `"category"`
#' - `"ncbiTaxonomyId"`
#' - `"ncitDiseaseId"`
#' - `"ncitDiseaseName"`
#' - `"oncotreeCode"`
#' - `"oncotreeMainType"`
#' - `"oncotreeName"`
#' - `"oncotreeTissue"`
#' - `"organism"`
#' - `"sexOfCell"`
#'
#' @return Modified object, subset to only contain matching cell lines, which
#' are defined in the rows.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' print(dim(object))
#' subset <- selectCells(
#'     object = object,
#'     category = "Cancer cell line",
#'     organism = "Homo sapiens",
#'     sexOfCell = "Female"
#' )
#' print(dim(subset))
NULL



## Updated 2023-09-12.
`selectCells,Cellosaurus` <-
    function(object, ...) {
        args <- list(...)
        assert(
            validObject(object),
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        assert(isSubset(
            x = names(args),
            y = c(
                "category",
                "ncbiTaxonomyId",
                "ncitDiseaseId",
                "ncitDiseaseName",
                "oncotreeCode",
                "oncotreeMainType",
                "oncotreeName",
                "oncotreeTissue",
                "organism",
                "sexOfCell"
            )
        ))
        df <- as(object, "DFrame")
        df <- df[, names(args), drop = FALSE]
        df <- decode(df)
        list <- Map(
            col = names(args),
            arg = args,
            MoreArgs = list("df" = df),
            f = function(col, arg, df) {
                vals <- df[[col]]
                if (is(vals, "List")) {
                    lgl <- any(vals %in% arg)
                } else {
                    lgl <- vals %in% arg
                }
                which(lgl)
            }
        )
        i <- sort(Reduce(f = intersect, x = list))
        assert(
            hasLength(i),
            msg = "No cell lines to select."
        )
        out <- object[i, , drop = FALSE]
        out
    }



#' @rdname selectCells
#' @export
setMethod(
    f = "selectCells",
    signature = signature(object = "Cellosaurus"),
    definition = `selectCells,Cellosaurus`
)
