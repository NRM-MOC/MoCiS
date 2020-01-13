##' Subsets the data based on the selection rules.
##'
##' None
##' @title subset_age
##' @param data A \code{data.frame}.
##' @param agesel A list from age_select() or similar.
##' @param what A variable.
##' @param genus The species.
##' @param where The locale.
##' @return Data subsetted on age.
##' @author Erik Lampa
##' @export
subset_age <- function(data, agesel, what, genus, where) {
    ## Performs age selection where applicable. Checks the list agesel if there
    ## is an entry for locale "where", genus "genus" and variable "what". If
    ## there are limits, extract them and subset the data based on the limits.
    lims <- getElement(getElement(getElement(agesel, what), genus), where)
    if (!is.null(lims)) {
        subset(data, as.numeric(ALDR) >= lims[1] &
               as.numeric(ALDR) <= lims[2])
    } else data
}
