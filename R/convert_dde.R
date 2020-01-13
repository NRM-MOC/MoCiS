##' Conversion rules for DDE
##'
##' For use in \code{convert}. Produces a list with species names as the
##'     elements. Each element is itself a list with values for the locales.
##' @title convert_dde()
##' @param ... Ignored
##' @return A list with conversion rules
##' @author Erik Lampa
##' @examples ## Change the value for Herring at Landsort to 0.5
##'     conv_dde <- convert_dde()
##'     conv_dde$CLUP$LAND <- 0.5
##' @export
convert_dde <- function(...) {
    ## Sets the rules for conversion between labs.
    list(CLUP = list(HAFJ = 1,
                     ANGK = 1,
                     ANGV = 1,
                     LAND = 0.72,
                     UTLA = 0.96,
                     UTLV = 0.96,
                     FLAD = 1.21),
         GADU = list(SEGO = 0.91,
                     FLAD = 0.96),
         SIGR = list(STKA = 0.99),
         MYTI = list(NIDI = 0.64,
                     FJBA = 0.82))
}
