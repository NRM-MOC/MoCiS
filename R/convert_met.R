##' Conversion rules for metals
##'
##' For use in \code{convert}. Produces a list with contaminant names as the
##'     elements. Each element is itself a list with values for the species.
##' @title convert_met()
##' @param ... Ignored
##' @return A list with conversion rules
##' @author Erik Lampa
##' @examples ## Change the value for Herring and Hg to 1.25
##'     conv_met <- convert_met()
##'     conv_met$HG$CLUP <- 1.25
##' @export
convert_met <- function(...) {
    ## Sets the rules for conversion between labs.
    list(HG = list(GADU = 1.19,
                   SIGR = 1.25),
         PB = list(CLUP = 1.4,
                   GADU = 3.06,
                   PERC = 2.31),
         CD = list(CLUP = 1.14,
                   GADU = 1.85,
                   PERC = 1.50,
                   MYTI = 0.74),
         NI = list(CLUP = 1.97),
         CR = list(CLUP = 2.01,
                   MYTI = 2.48,
                   SIGR = 9.19),
         CU = list(CLUP = 0.89,
                   GADU = 1.35,
                   PERC = 1.35,
                   MYTI = 0.74),
         ZN = list(CLUP = 1.13),
         AG = list(MYTI = 0.74),
         SE = list(MYTI = 0.74)
         )
}
