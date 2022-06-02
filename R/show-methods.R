#' Show an object
#' @name show
#' @inherit methods::show params return title
#' @keywords internal
#' @note Updated 2022-06-02.
NULL



## Updated 2022-06-02.
`show,Cellosaurus` <- # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            "dataVersion" = metadata(object)[["dataVersion"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "Cellosaurus"),
    definition = `show,Cellosaurus`
)
