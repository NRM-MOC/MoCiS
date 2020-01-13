##' Converts values below the LOD
##'
##' For internal use only
##' @title lod
##' @param v A varaible.
##' @param fun A function.
##' @return A value.
##' @author Erik Lampa
##' @export
lod <- function(v, fun = NULL) {
    ## Values below the LOD are set to LOD/sqrt(2)
    
    if (is.null(fun)) fun <- function(x) abs(x)/sqrt(2)
    ifelse(v < 0, fun(v), v)
}
