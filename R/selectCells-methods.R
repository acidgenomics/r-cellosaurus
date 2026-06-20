#' @name selectCells
#' @inherit AcidGenerics::selectCells
#' @note Updated 2023-09-12.
#'
#' @details
#' Only exact matching is currently supported.
#'
#' @section Supported keys:
#'
#' - `"btoId"`
#' - `"category"`
#' - `"isCancer"`
#' - `"isContaminated"`
#' - `"isProblematic"`
#' - `"ncbiTaxonomyId"`
#' - `"ncitDiseaseId"`
#' - `"ncitDiseaseName"`
#' - `"oncotreeCode"`
#' - `"oncotreeMainType"`
#' - `"oncotreeName"`
#' - `"oncotreeTissue"`
#' - `"organism"`
#' - `"sexOfCell"`
#' - `"uberonId"`
#'
#' @return Modified object, subset to only contain matching cell lines.
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


## Updated 2026-06-02.
`selectCells,Cellosaurus` <- # nolint
    function(object, ...) {
        args <- list(...)
        validCols <- c(
            "btoId",
            "category",
            "isCancer",
            "isContaminated",
            "isProblematic",
            "ncbiTaxonomyId",
            "ncitDiseaseId",
            "ncitDiseaseName",
            "oncotreeCode",
            "oncotreeMainType",
            "oncotreeName",
            "oncotreeTissue",
            "organism",
            "sexOfCell",
            "uberonId"
        )
        assert(
            validObject(object),
            isSubset(validCols, colnames(object)),
            hasLength(args),
            hasNames(args),
            isSubset(names(args), validCols),
            all(bapply(X = args, FUN = function(x) is.atomic(x) || is.null(x)))
        )
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
            msg = "No cell lines matched selection criteria."
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
