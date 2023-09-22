#' @name export
#' @inherit pipette::export
#' @note Updated 2023-09-22.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `DFrame` method.
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



## Updated 2023-09-20.
`export,Cellosaurus` <- # nolint
    function(object, con, ...) {
        assert(
            validObject(object),
            isString(con)
        )
        df <- as(object, "DFrame")
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
        con = "character"
    ),
    definition = `export,Cellosaurus`
)
