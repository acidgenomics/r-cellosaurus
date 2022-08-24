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



## Updated 2022-08-23.
`export,Cellosaurus` <- # nolint
    function(object,
             con,
             format,
             ...) {
        validObject(object)
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
        args <- list()
        args[["object"]] <- df
        if (!missing(con)) {
            args[["con"]] <- con
        }
        if (!missing(format)) {
            args[["format"]] <- format
        }
        args <- append(x = args, values = list(...))
        do.call(what = export, args = args)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Cellosaurus",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,Cellosaurus`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Cellosaurus",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,Cellosaurus`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Cellosaurus",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,Cellosaurus`
)
