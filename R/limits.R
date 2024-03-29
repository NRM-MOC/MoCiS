##' Sets the limits to be used in the plots
##'
##' For internal use only
##' @title limits
##' @param ... Ignored.
##' @return A list with limits for the contaminants.
##' @author Erik Lampa
##' @export
limits <- function(...) {
    ## Sets limit values.
    list(
        HG = 24, #20
        PB = 0.3, #0.085,
        PBlimn = 0.085, #1.1,
        CD = 2.6, #6.65,
        CB118 = 0.024,
        CB153 = 1.6,
        DDE = 0.005,
        AHCH = 0.0026,
        BHCH = 0.0026,
        LINDA = 0.0026,
        HCB = 0.01,
        PFOS = 153, #155,
        TCDDEQV = c(3.5, 0.6),
        BDE47 = 0.0085,
        # BDE99 = 0.0085,
        # BDE153 = 0.0085,
        HBCD = 167,
        ANT = 290,
        BAA = 80,
        BAP = 600,
        BGHIP = 110,
        NAP = 340,
        PA = 1700,
        FLU = 110,
        PYR = 100)
}
