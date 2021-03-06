##' Creates a 2d map showing the average concentrations of the contaminants for
##' each locale.
##'
##' Spring locales are excluded. If there are \code{cuts} or less unique values those
##'     values are plotted, otherwise the 5th, 25th, 50th, 75th and 95th
##'     percentiles are plotted. Locales where the average concentration exceeds
##'     the limit are plotted with a red line around the circle.
##' @title mocis2d
##' @param mocis The output from \code{mocis}.
##' @param var The contaminant of interest.
##' @param genus The species of interest.
##' @param year Which year should be the latest?
##' @param latest How many years should the average value be based on? Defaults
##'     to 3.
##' @param domain The section of the map to be plotted. A length four vector
##'     consisting of longitude (west), longitude (east), latitude (smouth),
##'     latitude (north).
##' @param bg.col Ocean color.
##' @param land.col Land color.
##' @param shapefile Path to a shapefile if the borders should be plotted using a specific shapefile.
##'     Defaults to NULL.
##' @param statnames A list with names and coordinates of the stations. Defaults to \code{stations()}.
##' @param border A logical. Should country borders be plotted? Deafualts to TRUE
##' @param size The maximum size of the circles. Defaults to 5.
##' @param col.limit The color of the line around the circle when the limit is exceeded.
##' @param xoff Adjustment to the plot area. Deafults to adding and subtracting
##'     1 from the longitudes.
##' @param yoff Adjustment to the plot area. Deafults to adding and subtracting
##'     1 from the latitudes.
##' @param textcoord Not used.
##' @param frac Not used.
##' @param max.lwd The maximum line with around the circle.
##' @param cuts Number of values to plot in the legend. Defaults to 5.
##' @param specs A list with species names. Defaults to \code{species{}}.
##' @param xpos Not used.
##' @param ypos Not used.
##' @param cex.text Size of the margin text.
##' @param plotlabels A list with plot labels. Defaults to \code{plotlabs()}.
##' @param cex.mtext Character expansion for the margin text. Defaults to 0.8.
##' @param cex.legend Character expansion for the legend text. Defaults to 1
##' @param pos.legend A length 2 vector describing the (x, y) position of the
##'     legend.
##' @param mtext.at Placement of the margin text. Defaults to par("usr")[1].
##' @param ... Other arguments passed to \code{plot()}, e.g. cex.axis to change
##'     the character expansion for the axis tick labels.
##' @return A plot.
##' @author Erik Lampa
##' @export
##' @import prevR dplyr
mocis2d <- function(mocis, var, genus, year = NULL, latest = 3,
                    domain = c(9, 27, 54, 70),
                    bg.col = qualitative_hcl(3, "Pastel 1", l = 90)[3],
                    land.col = sequential_hcl(3, "PinkYl")[3],
                    shapefile = NULL, statnames = stations(), border = TRUE,
                    size = 5, col.limit = diverging_hcl(2)[2],
                    xoff = c(1, -1), yoff = c(1, -1),
                    textcoord = NULL, frac = 0.8, max.lwd = 4,
                    cuts = 5, specs = species(), xpos = 0.05,
                    ypos = 0.95, cex.text = 0.8, plotlabels = plotlabs(),
                    cex.mtext = 0.8, cex.legend = 1, pos.legend = NULL,
                    mtext.at = NULL, ...) {
    temp <- statnames
    ## Generates a 2d map. Use TMWorldbordes from prevR if a shapefile is not provided    
    if (is.null(shapefile)) {
        borders <- subset_shape(fortify_shape(prevR::TMWorldBorders),
                                domain = domain + c(-4, 4, -4, 4))
    }
    if (!is.null(shapefile)) {
        borders <- fortify_shape(readOGR(dsn = shapefile))
    }
    ## Use prevR::TMWorldborders for borders even if a shapefile is provided
    if (!is.null(shapefile) & border == TRUE) {
        borders2 <- fortify_shape(prevR::TMWorldBorders)
    }
    if (exists("borders2")) {
        borders2<- subset_shape(borders2, domain)
    }
    ## Set the plot limits as a shrunken domain to avoid line artefacts
    xlims <- domain[1:2] + xoff
    ylims <- domain[3:4] + yoff

    ## Just to get the stattype, can probably be coded better
    li <- lapply(mocis, function(x) {
        getElement(x, genus)
    })
    li <- Filter(Negate(is.null), li)
    type <- na.omit(unlist(lapply(li, function(x) {
        if (is.list(x)) {
            getElement(x[[var]], "stattype")[1]
        } else NA
    })))[1]

    ## If type does not exist means that the variable does not exist. Just
    ## return NULL and be done with it.
    if (is.na(type)) return(NULL)

    ## For bilds, use the marine stations
    if (type == "uria") type <- "hav"

    ## Text coordinates not used anymore but may break something else
    if (is.null(textcoord) & type == "hav") {
        textcoord <- c(15, 60.5)
    } else textcoord <- c(20, 59)

    ## Get the station names
    statnames <- do.call("rbind.data.frame", lapply(statnames[[type]], function(x) {
        unlist(x)
    }))
    names(statnames) <- c("stationid", "NKOO", "EKOO")
    ## Coded as factors...
    statnames$NKOO <- as.numeric(as.character(statnames$NKOO))
    statnames$EKOO <- as.numeric(as.character(statnames$EKOO))
    statnames$stationid <- as.character(statnames$stationid)
    ## Convert NKOO and EKOO to lat and long
    latlong <- ne2latlong(statnames$NKOO, statnames$EKOO)
    statnames$lat <- latlong[,1]
    statnames$long <- latlong[,2]

    ## Now, get the relevant data from the mocis object
    a <- lapply(mocis, function(x) {
        lapply(x, function(y) {
            foo <- try(getElement(y, var)$aggdata,
                       silent = TRUE)
            if (is.data.frame(foo)) {
                if (nrow(foo[foo$YEAR <= year,]) >= latest) {
                    subset(foo[foo$YEAR <= year,], YEAR > (year - latest))
                }
            } else NA
        })
    })
    a <- lapply(a, function(x) getElement(x, genus))
    ## Check if a data frame is returned. If not, an error has occured
    ok <- unlist(lapply(a, function(x) ifelse(!is.data.frame(x), 0, 1)))
    a <- a[ok == 1]
    a <- do.call(dplyr::bind_rows, a)
    a <- subset(a, !is.na(a$YEAR))

    if (all(is.na(a$NKOO)) | all(is.na(a$EKOO))) {
        a$NKOO <- sapply(a$LOC, function(x) temp[[type]][[x]]$NKOO)
        a$EKOO <- sapply(a$LOC, function(x) temp[[type]][[x]]$EKOO)
    }
    
    ## Get the limit values if applicable
    lims <- lapply(mocis, function(x) {
        lapply(x, function(y) {
            foo <- try(getElement(y, var), silent = TRUE)
            if (!inherits(foo, "try-error")) {
                foo$limit$tv
            }
        })
    })
    bar <- lapply(lims, function(x) getElement(x, genus))
    bar <- Filter(Negate(is.null), bar)
    lims <- do.call("c", lapply(bar, function(x) ifelse(length(x) > 1, x[1], x)))

    if (!all(is.na(lims))) {
        lims <- data.frame(limit = lims, LOC = names(lims))
    }

    ## Calculate the geometric mean for each locale
    varaggfmla <- as.formula(paste0(var, " ~ LOC"))
    aa <- aggregate(varaggfmla, FUN = geoMean, data = a)

    ## Dabble around with lat and long again
    latlong2 <- ne2latlong(a$NKOO, a$EKOO)
    a$lat <- latlong2[,1]
    a$long <- latlong2[,2]

    aala <- aggregate(lat ~ LOC, FUN = unique, data = a)
    aalo <- aggregate(long ~ LOC, FUN = unique, data = a)

    aa <- merge(aa, aala, by = "LOC")
    aa <- merge(aa, aalo, by = "LOC")

    ## If there are limits, attach them to the data, else set the limit to 1e6
    if (!all(is.na(lims))) {
        aa <- merge(aa, lims, by = "LOC", all.x = TRUE)
    } else aa$limit <- 1e6

    ## Borders are defined by groups in the borders object
    grs <- levels(droplevels(borders$group))
    if (exists("borders2")) {
        grs2 <- levels(droplevels(borders2$group))
    }
    ## Drop the spring locales
    aa <- subset(aa, substr(LOC, 4, 4) != "V")
    aa <- aa[order(aa[[var]]),]
    ## And calculate the ecdf
    pct <- ecdf(aa[[var]])

    ## When there are many values, calculate percentiles and generate five color values
    if (length(unique(aa[[var]])) >= cuts) {
        sizes <- seq(1, size, length = 100)
        q <- pct(aa[[var]])
        o <- ceiling(100 * q)
        aa$size <- sizes[o]
        col <- rev(sequential_hcl(n = 100, palette = "Blues", l = c(30, 100)))
        aa$col <- col[o]
        ## Generate the legend
        leg <- list(text = quantile(aa[[var]],
                    c(0.05, 0.25, 0.5, 0.75, 0.95)),
                    size = sizes[c(5, 25, 50, 75, 95)],
                    col = col[c(5, 25, 50, 75, 95)])
    }
    ## When there are less than cuts unique values. Do something similar
    if (length(unique(aa[[var]])) < cuts) {
        size <- seq(1 + 2/size, size,
                    length = length(unique(aa[[var]])))
        col <- rev(sequential_hcl(n = length(unique(aa[[var]])),
                                     palette = "Blues",
                                     l = c(30, 100)))
        temp <- data.frame(xx = sort(unique(aa[[var]])),
                           size = size, col = col)
        ind <- which(names(aa) == var)
        names(temp)[1] <- names(aa)[ind]
        aa <- merge(aa, temp, by = var)
        leg <- list(text = unique(temp[[var]]),
                    size = temp$size, col = temp$col)
    }

    ## Set up line width and color if mean value > LOD
    aa$lwd <- ifelse(aa[[var]] < aa$limit, 2,
                     max.lwd)
    aa$col.line <- ifelse(aa[[var]] < aa$limit, "black", col.limit)

    ## Now plot. Start by setting up the plot area
    par(mar = c(5, 4, 4, 8) + 0.1)
    plot(lat ~ long, data = borders,
         xlim = xlims, ylim = ylims, type = "n",
         xlab = "Longitude", ylab = "Latitude",
         las = 1, ...)
    ## Probably not used
    if (exists("borders2")) {
        for (i in 1:length(grs2)) {
        do2 <- subset(borders2, group == grs2[i])
        with(do2,
             lines(long, lat,
                   type = "l", lty = 1,
                   col = "grey10"))
        }}
    ## Draw a rectangle covering the plot area with blue
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = bg.col)
    xvals <- split(borders$long, borders$group)
    yvals <- split(borders$lat, borders$group)
    ## Add the land
    mapply(polygon, xvals, yvals, col = land.col)
    xvalsl <- split(lakes$long, lakes$group)
    yvalsl <- split(lakes$lat, lakes$group)
    ## And the lakes
    mapply(polygon, xvalsl, yvalsl, col = bg.col)

    ## Plot the stations
    points(aa$long, aa$lat,
           pch = 21, bg = adjustcolor(aa$col, alpha.f = 0.9),
           col = adjustcolor(aa$col.line, alpha.f = 0.9),
           lwd = aa$lwd,
           cex = aa$size)
    ## Tick marks
    rug(xlims[1]:(xlims[2]), ticksize = -0.01, side = 1)
    rug(ylims[1]:(ylims[2]), ticksize = -0.01, side = 2)

    ## Get the plot labels based on the species
    if (genus %in% c("CLUP", "PERC", "GADU", "ZOAR", "ESOX", "SALV", "RUTI")) {
        label <- getElement(getElement(plotlabels, "fish"), var)
    }
    if (genus == "MYTI") {
        label <- getElement(getElement(plotlabels, "bluemussel"), var)
    }
    if (genus %in% c("SIGR", "HAEM", "STER")) {
        label <- getElement(getElement(plotlabels, "birds"), var)
    }
    spec <- getElement(specs, genus)$name

    ## Create the margin text
    minyr <- min(a$YEAR)
    maxyr <- max(a$YEAR)
    labtext <- bquote(bold(.(spec))*","~bold(.(label))*","~
                          bold(.(minyr))*" - "*bold(.(maxyr)))
    mtext(labtext, line = 1, 
          at = ifelse(is.null(mtext.at), par("usr")[1], mtext.at),
          cex = cex.mtext, font = 2, adj = 0, padj = 1)
    par(xpd = TRUE)
    ## And finally the legend
    legend(x = ifelse(is.null(pos.legend), domain[2] + 0.5, pos.legend[1]),
           y = ifelse(is.null(pos.legend),
                      mean(domain[3:4] + min(nrow(aa), cuts)/2 - 1/2),
                      pos.legend[2]),
           legend = rev(signif(leg$text, 2)),
           bty = "n",
           x.intersp = 2,
           y.intersp = 2, pch = 21,
           col = "black",
           pt.bg = rev(adjustcolor(leg$col, alpha.f = 0.9)),
           pt.cex = rev(leg$size),
           pt.lwd = 2, cex = cex.legend)
    ## Add a black rectangle around the map
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4])
    par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)
}
