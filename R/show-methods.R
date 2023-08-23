#' Show an object
#' @name show
#' @inherit methods::show params return title
#' @keywords internal
#' @note Updated 2023-08-23.
NULL



## Updated 2023-08-23.
`show,Cellosaurus` <- # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            "cells" = nrow(object),
            "release" = majorVersion(metadata(object)[["dataVersion"]]),
            "date" = metadata(object)[["date"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "Cellosaurus"),
    definition = `show,Cellosaurus`
)
