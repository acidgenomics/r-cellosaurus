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
        drop <- c(
            "ancestors",
            "comment",
            "creationDate",
            "obsolete",
            "subset",
            "xref"
        )
        keep <- !colnames(df) %in% drop
        df <- df[, keep]
        export(
            object = df,
            con = con,
            format = format,
            ...
        )
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Cellosaurus",
        con = "character",
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
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,Cellosaurus`
)
