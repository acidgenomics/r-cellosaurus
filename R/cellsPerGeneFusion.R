#' Cells per gene fusion
#'
#' @export
#' @note Updated 2023-10-05.
#'
#' @inheritParams geneFusions
#'
#' @return `matrix`.
#' Logical matrix.
#' Gene fusion pairs in columns, cells in rows.
#'
#' @examples
#' data(cello)
#' mat <- cellsPerGeneFusion(object, format = "geneName")
#' print(mat[1L:5L, 1L:5L])
cellsPerGeneFusion <-
    function(object,
             fusionFormat = c("geneName", "hgncId")) {
        assert(validObject(object))
        fusionFormat <- match.arg(fusionFormat)
        cl <- geneFusions(object = object, format = fusionFormat)
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
