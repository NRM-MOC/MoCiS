##' Checks if data should be fat adjusted
##'
##' Only for internal use
##' @title check_fat_adjust
##' @param data A data frame.
##' @param y A variable.
##' @param locvar Variable name for the locale variable.
##' @param fatadjust A list with variables, species and locale combinations that
##'     should be fat adjusted 
##' @return 1 if fat adjustment should be made, 0 otherwise.
##' @author Erik Lampa
##' @export
check_fat_adjust <- function(data, y, locvar, fatadjust = fat_adjust()) {
    ## Checks if fat adjustment should be done
    
    out <- lapply(fatadjust, function(x) {
        if (length(x) == 2) {
            ifelse(x$VAR == y & x$GENUS == unique(data$GENUS), 1, 0)
        } else if (length(x) == 3) {
            ifelse(x$VAR == y &
                   x$GENUS == unique(data$GENUS) &
                 unique(data[[locvar]]) %in% x$LOC, 1, 0)
        }
    })
    ifelse(any(out == 1), 1, 0)
}
