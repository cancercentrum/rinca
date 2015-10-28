
## This method is just a wrapper if x is neither integer, nor pin.
## It is not documented since it should not be used purposely.
#' @export
rpin.numeric <- function(x, ...) {
    if (suppressWarnings(!any(is.na(as.pin(x))) && is.pin(as.pin(x)))) {
        rpin(as.pin(x), ...)
    }
    else if (length(x) == 1) {
        if ( abs(x - round(x)) >= .Machine$double.eps^0.5) {
            warning("x rounded to nearest lower integer!")
        }
        rpin(as.integer(x), ...)
    }
}
