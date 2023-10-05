#' Cells per mutation
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @inheritParams mutations
#'
#' @return `matrix`.
#' Logical matrix.
#' Genes in columns, cells in rows.
#'
#' @seealso
#' - `stringi::stri_list2matrix`.
#'
#' @examples
#' data(cello)
#' mat <- cellsPerMutation(object, format = "geneName")
#' print(mat[1L:5L, 1L:5L])
cellsPerMutation <-
    function(object,
             format = c("geneName", "hgncId", "both")) {
        assert(validObject(object))
        format <- match.arg(format)
        cl <- mutations(object = object, format = format)
        assert(is(cl, "CharacterList"))
        i <- names(cl)
        j <- sort(unique(unlist(cl)))
        ## FIXME Is there a more performant want to do this with CL?
        lst <- mcMap(
            f = `%in%`,
            table = cl,
            MoreArgs = list("x" = j),
            USE.NAMES = FALSE
        )
        mat <- do.call(what = rbind, args = lst)
        dimnames(mat) <- list(i, j)
        mat
    }
