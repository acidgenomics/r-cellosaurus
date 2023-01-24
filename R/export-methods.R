#' @name export
#' @inherit pipette::export
#' @note Updated 2023-01-24.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `DataFrame` method.
#'
#' @examples
#' data(cello)
#'
#' ## Cellosaurus ====
#' object <- cello
#' tempdir <- AcidBase::tempdir2()
#' con <- file.path(tempdir, "cello.csv")
#' x <- export(object = object, con = con)
#' print(x)
#' AcidBase::unlink2(tempdir)
NULL



## Updated 2023-01-24.
`export,Cellosaurus` <- # nolint
    function(object,
             con,
             format, # missing
             ...) {
        if (missing(format)) {
            format <- NULL
        }
        assert(
            validObject(object),
            isString(con),
            is.null(format)
        )
        df <- as(object, "DataFrame")
        dropCols <- c(
            "comments",
            "crossReferences",
            "date",
            "diseases",
            "hierarchy",
            "originateFromSameIndividual",
            "referencesIdentifiers",
            "strProfileData",
            "webPages"
        )
        assert(isSubset(dropCols, colnames(df)))
        cols <- setdiff(x = colnames(df), y = dropCols)
        df <- df[, cols]
        export(object = df, con = con, ...)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Cellosaurus",
        con = "character",
        format = "missing"
    ),
    definition = `export,Cellosaurus`
)
