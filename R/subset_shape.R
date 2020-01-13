##' Helper function.
##'
##' Taken from https://rpubs.com/danielequs/199150
##' @title subset_shape
##' @param x A \code{data.frame}.
##' @param domain The domain.
##' @return Subsetted map info.
##' @author Erik Lampa
##' @export
subset_shape <- function(x, domain) {

    ## Helper function. Taken from https://rpubs.com/danielequs/199150
    
    x.subset <- subset(x, long > domain[1] & 
                          long < domain[2] & 
                          lat > domain[3] & 
                          lat < domain[4])
    x.subset
}
