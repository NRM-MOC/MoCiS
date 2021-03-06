##' Checks if a variable exists before generating tables
##'
##' For internal use only
##' @title checkvar
##' @param mocis Output from \code{mocis}
##' @param var A variable
##' @param genus Species
##' @return 1 if the variable exists, 0 otherwise
##' @author Erik Lampa
##' @export
checkvar <- function(mocis, var, genus) {
    
    out <- lapply(1:length(mocis), function(i) {
        a <- try(getElement(getElement(mocis[[i]], genus), var),
                 silent = TRUE)
        if (!inherits(a, "try-error") & !is.null(a)) {
            1
        } else 0
    })
    out <- unlist(out)
    ifelse(any(out == 1), 1, 0)
}
