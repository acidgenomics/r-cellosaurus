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
#' con <- AcidBase::tempdir2()
#' x <- export(object = object, con = con)
#' print(x)
#' AcidBase::unlink2(con)
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
        con = "ANY",
        format = "ANY"
    ),
    definition = `export,Cellosaurus`
)
