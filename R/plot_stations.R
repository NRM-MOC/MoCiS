##' Plots a list of stations and numbers them.
##'
##' None
##' @title plot_stations
##' @param type A character describing which stations to plot. Either
##'     \code{"hav"} for marine stations or \code{"limn"} for
##'     limnic stations.
##' @param legend Either \code{"right"} to place the legend with the station
##'     names on the right side of the plot or \code{"none"} to suppress the
##'     legend. Defaults to \code{"none"}.
##' @param bg.col Sea color.
##' @param land.col Land color.
##' @param col.point Color which the points should be filled with.
##' @param cex.point Expansion factor for the points.
##' @param stationnames A list with station names. Defaults to \code{stations()}.
##' @param statord The ordering of the stations. Defaults to \code{statorder}.
##' @param domain The section of the map to be plotted. A length four vector
##'     consisting of longitude (west), longitude (east), latitude (smouth),
##'     latitude (north).
##' @param xoff Adjustment to the plot area. Deafults to adding and subtracting
##'     1 from the longitudes.
##' @param yoff Adjustment to the plot area. Deafults to adding and subtracting
##'     1 from the latitudes.
##' @param legend.pos A length 2 vector of x and y coordinates. The position of
##'     the legend if plotted.
##' @param marg Set the plot margins if the legend is plotted. Defaults to
##' \code{c(7, 4, 4, 12) + 0.1} .
##' @param yspace Space between the lines in the legend if plotted.
##' @param legend.cex Character expansion of the legend if plotted.
##' @return A plot.
##' @author Erik Lampa
##' @export
##' @import rgeos prevR
plot_stations <- function(type = NULL, legend = c("right", "none"),
                          bg.col = qualitative_hcl(3, "Pastel 1", l = 90)[3],
                          land.col = sequential_hcl(3, "PinkYl")[3],
                          col.point = qualitative_hcl(3, "Set 2")[3],
                          cex.point = 2.5,
                          stationnames = stations(),
                          statord = statorder(),
                          domain = c(9, 27, 54, 70),
                          xoff = c(1, -1), yoff = c(1, -1),
                          legend.pos = NULL,
                          marg = c(7, 4, 4, 12) + 0.1,
                          yspace = 1.5, legend.cex = 1) {
    if (!type %in% c("hav", "limn")) {
        stop("type must be either hav or limn")
    }
    if (is.null(legend.pos)) {
        legend.pos <- domain[c(2, 4)]
    }
    if (length(legend) == 2) {
        legend <- "none"
    }
    xlims <- domain[1:2] + xoff
    ylims <- domain[3:4] + yoff
    stat.order <- data.frame(loc = statord[[type]][, "loc"])
    dup <- duplicated(stat.order$loc)
    stat.order <- subset(stat.order, dup == FALSE)
    if (type == "hav") {
        spr <- ifelse(substr(stat.order$loc, 4, 4) == "V", 1, 0)
        stat.order <- subset(stat.order, spr == 0)
    }
    stat.order$order <- 1:nrow(stat.order)
    borders <- subset_shape(fortify_shape(prevR::TMWorldBorders),
                            domain = domain + c(-2, 2, -2, 2))
    loc <- names(stationnames[[type]])
    stationnames <- do.call("rbind.data.frame", lapply(stationnames[[type]], function(x) {
        unlist(x)
    }))
    names(stationnames) <- c("stationid", "NKOO", "EKOO")
    stationnames$NKOO <- as.numeric(as.character(stationnames$NKOO))
    stationnames$EKOO <- as.numeric(as.character(stationnames$EKOO))
    stationnames$stationid <- as.character(stationnames$stationid)
    stationnames$loc <- loc
    latlong <- ne2latlong(stationnames$NKOO, stationnames$EKOO)
    stationnames$lat <- latlong[,1]
    stationnames$long <- latlong[,2]
    stationnames <- merge(stat.order, stationnames, by = "loc")
    stationnames <- stationnames[order(stationnames$order),]
    grs <- levels(droplevels(borders$group))
    if(legend == "right") {
        par(mar = marg)
    } else {
        par(mar = c(7, 5, 4, 8) + 0.1)
    }

    plot(lat ~ long, data = borders, col = "blue",
         xlim = xlims, ylim = ylims, type = "n",
         xlab = "Longitude", ylab = "Latitude",
         las = 1)
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = bg.col)
    xvals <- split(borders$long, borders$group)
    yvals <- split(borders$lat, borders$group)
    mapply(polygon, xvals, yvals, col = land.col)
    if (type == "limn") {
        stationnames$long[23] <- stationnames$long[23] - 0.15
        stationnames$lat[23] <- stationnames$lat[23] - 0.15
    }
    xvalsl <- split(lakes$long, lakes$group)
    yvalsl <- split(lakes$lat, lakes$group)
    mapply(polygon, xvalsl, yvalsl, col = bg.col)
    points(stationnames$long, stationnames$lat,
           bg = col.point,
           pch = 21, col = adjustcolor("black", alpha.f = 0.5),
           cex = cex.point)
    text(stationnames$long, font = 2,
         stationnames$lat,
         labels = stationnames$order,
         cex = 0.7)
    par(xpd = TRUE)
    if (legend == "right") {
        legend(x = legend.pos[1],
               y = legend.pos[2],
               legend = paste0(stationnames$order, ". ", stationnames$stationid),
               cex = legend.cex, y.intersp = yspace,
               bty = "n")
    }
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4])
    par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)
}
