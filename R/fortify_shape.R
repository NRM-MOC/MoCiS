##' Sets shape data up for plotting
##'
##' Taken from https://rpubs.com/danielequs/199150
##' @title fortify_shape
##' @param x Output from readOGR or similar
##' @return A data frame
##' @author https://rpubs.com/danielequs/199150
##' @importFrom dplyr inner_join
##' @importFrom ggplot2 fortify
##' @export
fortify_shape <- function(x) {

    ## Helper function. From https://rpubs.com/danielequs/199150
    
    x@data$id <- rownames(x@data)
    x.f <- fortify(x, region = "id")
    x.join <- inner_join(x.f, x@data, by = "id")
}
