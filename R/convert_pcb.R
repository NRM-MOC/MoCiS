##' Sets conversion rules for SUMPCB
##'
##' None
##' @title convert_pcb
##' @param ... Ignored
##' @return A data frame with Species, Locales, r1 and r2 values
##' @author Erik Lampa
##' @export
convert_pcb <- function(...) {
    ## Sets the conversion rules for SUMPCB.
    data.frame(GENUS = c(rep("CLUP", 7), rep("GADU", 2),
                         rep("MYTI", 2), "SIGR"),
               LOC = c("HAFJ", "ANGK", "ANGV", "LAND",
                       "UTLA", "UTLV", "FLAD", "SEGO", "FLAD",
                       "NIDI", "FJBA", ""),
               r1 = c(0.14, 0.14, 0.13, 0.12, 0.12,
                      0.12, 0.13, 0.14, 0.15, 0.12, 0.12, 0.18),
               r2 = c(0.73, 0.83, 0.79, 0.61, 0.65, 0.67, 0.82,
                      0.69, 0.85, 0.74, 0.95, 0.77))
}
