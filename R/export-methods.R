#' @name export
#' @inherit pipette::export
#' @note Updated 2022-08-23.
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



## Updated 2022-09-13.
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
        cols <- setdiff(
            x = colnames(df),
            y = c(
                "ancestors",
                "comment",
                "creationDate",
                "obsolete",
                "subset",
                "xref"
            )
        )
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
