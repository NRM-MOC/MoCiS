##' Plots the data and the association between time and the contaminant
##'
##' The default subset is the 10 last years and it is written as such in the
##'     help files and output regardless of the actual value used.
##' @title plot_mocis
##' @param mocis Output from \code{mocis}.
##' @param var A character. The contaminant of interest.
##' @param loc A character. The locale of interest.
##' @param genus A character. The species of interest.
##' @param nyears An integer. The lowest number of years to plot. Defaults to 1.
##' @param col.line A vector of two colors. Which color is used depends on the
##'     p-value for the linear model.
##' @param lty.line A vector of length 2. The line types for the line. Which
##'     line type is used depends on the p-value for the linear model. Defaults
##'     to `c(1, 2)`.
##' @param lwd.line An integer. The line width for the regression line. Defaults
##'     to 2.
##' @param col.sm A vector of two colors. Which color is used depends on the
##'     p-value for the smooth.
##' @param lty.sm A vector of length 2. The line types for the smooth. Which
##'     line type is used depends on the p-value for the smooth. Defaults
##'     to \code{c(1, 2)}.
##' @param lwd.sm An integer. The line width for the smooth. Defaults
##'     to 2.
##' @param pval.line A vector of length 2. P-value cut-offs for when the line
##'     should be plotted. Defaults to `c(0.05, 0.1)`. If the p-value from
##'     the linear model fit is lower than the first, the first color is used
##'     and if the p-value is between the first and second p-values the second
##'     color is used.
##' @param pval.line10 A vector of length 2. P-value cut-offs for when the line
##'     for the last 10 years should be plotted. Defaults to `c(0.05, 0.1)`.
##'     If the p-value from the linear model fit is lower than the first,
##'     the first color is used and if the p-value is between the first and
##'     second p-values the second color is used. 
##' @param pval.sm A vector of length 2. P-value cut-offs for when the smooth
##'     should be plotted. Defaults to `c(0.05, 0.1)`. If the p-value from
##'     the smooth is lower than the first, the first color is used
##'     and if the p-value is between the first and second p-values the second
##'     color is used.
##' @param pch.point An integer. The point style for the yearly geometric mean
##'     values. Defaults to 16.
##' @param cex.point Expansion factor for the points representing the mean
##'     values. Defaults to 0.8.
##' @param col.point The color of the points.
##' @param col.lim A vector of length 2. The colors of the polygon describing
##'     the limits. If \code{var} is not TCDDEQV the first color is used throughout.
##' @param pch.data An integer. The point style for the raw data. Defaults to a
##'     dot (.)
##' @param cex.data Expansion factor for the raw data points. Defaults to 0.8.
##' @param col.data The color of the raw data points.
##' @param nobs An integer. The number of observations that should be present in
##'     a year for confidence intervals to be plotted. Defaults to 5.
##' @param main Not used.
##' @param plot.changep A logical. Should the change point be plotted (if applicable)?
##' @param xpos A value between 0 and 1. The position of the legend along the
##'     x-axis in Normalized Parent Coordinates. Defaults to 0.05.
##' @param ypos A value between 0 and 1. The position of the legend along the
##'     y-axis in Normalized Parent Coordinates. Defaults to 0.95.
##' @param cex.text Character expansion for the stats text. Defaults to 0.8.
##' @param cex.mtext Character expansion for the margin text. Defaults to 0.8.
##' @param mtext.at Position of the margin text. Defaults to par("usr")[1].
##' @param bgcol Not used.
##' @param ylim A vector of length 2. The y-axis limit.
##' @param fatadjust Not used.
##' @param what Determine what should be plotted provided the p-value(s) are in
##'     the specified range.  Can be one or more of:
##'     \code{"best"} only plot the best fit, 
##'     \code{"line"} only plot the line,
##'     \code{"smooth"} only plot the smooth,
##'     \code{"last10"} only plot the line for the last 10 years,
##'     \code{"all"} plot everything.
##' Defaults to "best" and "line10".
##' @param stats A list with station names. Defaults to \code{stations()}. 
##' @param specs A list with species names. Defaults to \code{species()}.
##' @param trafo A function. Transformation for the y-axis. Experimental.
##' @param outlier Set to NULL to suppress marking of suspected outliers.
##' @param col.rect Color of the bar indicating that all measurements are below
##'     the LOD.
##' @param force A logical. Force plotting even if the p-value(s) are not in the
##'     specified range? Defaults to FALSE. 
##' @param plotlabels A list with labels. Defaults to \code{plotlabs()}
##' @param statstext A logical. Controls whether to print the statistics text or
##'     not. Defaults to TRUE.
##' @param newlimit Set a new limit for the plot.
##' @param ... Other arguments to \code{plot()}.
##' @return A plot.
##' @author Erik Lampa
##' @export
##' @import colorspace
plot_mocis <- function(mocis, var, loc, genus, nyears = 1,
                       col.line = NULL, lty.line = c(1, 2),
                       lwd.line = 2,
                       col.sm = NULL, lty.sm = c(1, 2),
                       lwd.sm = 2,
                       pval.line = c(0.05, 0.1),
                       pval.line10 = c(0.05, 0.1),
                       pval.sm = c(0.05, 0.1),
                       pch.point = 16, cex.point = 0.8, col.point = "black",
                       col.lim = NULL, pch.data = 15, cex.data = 0.2,
                       col.data = "black",
                       nobs = 5, main = NULL, plot.changep = FALSE,
                       xpos = 0.05, ypos = 0.95, cex.text = 0.8,
                       cex.mtext = 0.8, mtext.at = NULL,
                       bgcol = "white", ylim = NULL, xlim = NULL, fatadjust = FALSE,
                       what = c("best", "line", "smooth", "last10", "all"),
                       stats = stations(), specs = species(), trafo = NULL,
                       outlier = "beta", col.rect = "grey90", force = FALSE,
                       plotlabels = plotlabs(), statstext = TRUE, newlimit = NA,
                       ...) {

    ## Main plot function. Plots the data and the regression line if the p-value
    ## is small.

    ## If what is empty, set the default to the best fit and the last 10 years
    if (length(what) == 5) what <- c("best", "line10")
    ## Not used since colorspace on CRAN is the required version or higher
    pkgv <- ifelse(packageVersion("colorspace") < "1.4-0", 1, 0)

    ## Allows for a transformation. Experimental
    if (is.null(trafo)) trafo <- function(x) x

    ## Set the default colors
    if(is.null(col.line)) {
        if (pkgv == 0) {
            col.line <- c(qualitative_hcl(5,"Dark 3")[1], ## Red
                          qualitative_hcl(5,"Dark 3")[4]) ## Blue
        } else {
            col.line <- c(brewer.pal(11, "Spectral")[2],
                          brewer.pal(11, "Spectral")[10])
        }
    }
    if(is.null(col.sm)) {
        if (pkgv == 0) {
            col.sm <- c(qualitative_hcl(5,"Dark 3")[1], ## Red
                        qualitative_hcl(5,"Dark 3")[4]) ## Blue
        } else {
            col.sm <- c(brewer.pal(11, "Spectral")[2],
                        brewer.pal(11, "Spectral")[10])
        }
    }
    if(is.null(col.lim)) {
        if (pkgv == 0) {
            col.lim <- c(qualitative_hcl(5,"Dark 3")[3],
                         qualitative_hcl(5, "Set 3 ")[4])
        } else {
            col.lim <- rep(brewer.pal(11, "Spectral")[8], 2)
        }
    }

    ## Get the data for plotting
    pldat <- try(getElement(getElement(getElement(mocis, loc), genus), var),
                 silent = TRUE)

    if (length(pldat) > 1 & !inherits(pldat, "try-error") & !is.null(pldat))
    {
        if (nrow(pldat$aggdata) >= nyears) {
            if (is.na(newlimit)) {
                limit <- pldat$limit$tv
                if (var == "TCDDEQVW") limit <- c(3.5, 0.6)
            } else limit <- newlimit
            fat <- pldat$limit$fat
            ## Calculate the confidence intervals for each years that contains
            ## nobs observations. This is just to prevent the CI to dominate the plot
            if (max(pldat$aggdata$n) > nobs) {
                pldat$aggdata$sd <- ifelse(is.na(pldat$aggdata$sd), 0,
                                           pldat$aggdata$sd)
                ## I forgot what this does...
                pldat$aggdata$n[pldat$aggdata$sd == 0] <- 1e6
                ## Calculate the actual CI
                lower <- exp(log(pldat$aggdata[[var]]) - qt(0.975, pldat$aggdata$n - 1) *
                             pldat$aggdata$sd / sqrt(pldat$aggdata$n))
                upper <- exp(log(pldat$aggdata[[var]]) + qt(0.975, pldat$aggdata$n - 1) *
                             pldat$aggdata$sd / sqrt(pldat$aggdata$n))
            }
            ## Set x-axis limits
            if (is.null(xlim)){
                xlims <- c(min(pldat$aggdata$YEAR) - 1, max(pldat$aggdata$YEAR) + 1)
                ## If less than five yeras exist in the data, set the limit to a
                ## reasonable value
                if (nrow(pldat$aggdata) < 5) {
                    xlims <- c(max(pldat$aggdata$YEAR) - 5.5, max(pldat$aggdata$YEAR) + 0.5)
                }
            }
            else
            {
                xlims <- xlim
            }
            ## Set the y-axis limit if left unspecified
            ## If an upper CI limit exists, set the limit to the largest upper
            ## CI limit plus 10%
            if (exists("upper") & is.null(ylim)) {
                ylims <- c(0, trafo(max(upper[pldat$aggdata$n > nobs], na.rm =
                                                                           TRUE, pldat$aggdata[[var]])) * 1.1)
                ## If no upper CI limit exists, set the limit to the largest
                ## data point plus 50%
            } else if (is.null(ylim)) {
                ylims <- c(0, trafo(max(pldat$data[[var]], na.rm = TRUE)) * 1.5)
            } else ylims <- ylim
            
            pldat$aggdata$loc <- paste0(genus, ", ", loc)
            ## Set some reasonable tick marks
            ## If the number of years < 30, set tick marks at every five years
            if (nrow(pldat$aggdata) < 30) {
                tck.at <- as.character(seq(min(pldat$aggdata$YEAR),
                                           max(pldat$aggdata$YEAR), by = 5))
            } else {
                ## Else, every ten years
                tck.at <- as.character(seq(min(pldat$aggdata$YEAR),
                                           max(pldat$aggdata$YEAR), by = 10))
            }
            ## Set the plot formula
            pfmla <- as.formula(paste0("trafo(", var, ") ~ YEAR"))
            ## Extract some useful statistics from the linear model if it exists
            if (!is.null(pldat$linmod)) {
                p.line <- pldat$linmod$p
                if (!is.null(pldat$linmod10)) {
                    p.line10 <- pldat$linmod10$p
                }
                ## And smooth
                p.sm <- pldat$smooth$p.smooth
                p.sm <- ifelse(is.na(p.sm), 1.01, p.sm)

                ## Figure out which lines to plot. If the smooth is a better fit
                ## than the linear model, plot it, else go with the linear model.
                plotline <- plotsmooth <- plotline10 <- FALSE
                if ("best" %in% what) {
                    if (p.sm >= 0.05) {
                        plotline <- TRUE
                    } else plotsmooth <- TRUE
                }
                if ("line10" %in% what) plotline10 <- TRUE
                if ("line" %in% what) plotline <- TRUE
                if ("smooth" %in% what) plotsmooth <- TRUE
                if ("all" %in% what) plotline <- plotsmooth <- plotline10 <- TRUE
            }
            ## Set the conditions determining when to plot the limit
            plotlimit <- ifelse(genus %in% c("CLUP", "PERC", "ZOAR", "GADU"),
                                TRUE,
                         ifelse(genus == "MYTI" & "pah" %in% pldat$stattype, TRUE,
                         ifelse("limn" %in% pldat$stattype & var %in% c("PB", "CD") &
                                genus != "PERC", FALSE,
                         ifelse("limn" %in% pldat$stattype, TRUE,
                                ifelse(!is.na(newlimit), TRUE, FALSE)))))
            ## Get the plot label
            if (genus %in% c("CLUP", "PERC", "GADU", "ZOAR", "PERC", "ESOX", "SALV")) {
                ylabel <- getElement(getElement(plotlabels, "fish"), var)
            }
            if (genus == "MYTI") {
                ylabel <- getElement(getElement(plotlabels, "bluemussel"), var)
            }
            if (genus %in% c("SIGR", "HAEM", "STER")) {
                ylabel <- getElement(getElement(plotlabels, "birds"), var)
            }
            ## Now the plot. First, set up an empty plot
            plot(pfmla, data = pldat$aggdata, pch = pch.point,
                 xlim = xlims, ylim = ylims, bty = "n",
                 type = "n", xaxs = "i", yaxs = "i", xlab = "Year",
                 ylab = ylabel,
                 las = 1, ...)
            ## Plot the limit if applicable
            if (plotlimit == TRUE & length(limit) == 1) {
                polygon(x = c(0, max(xlims),
                              max(xlims), 0),
                        y = c(0, 0, min(trafo(limit), ylims[2]),
                              min(trafo(limit), ylims[2])),
                        col = adjustcolor(col.lim[1], alpha.f = 0.3),
                        border = adjustcolor(col.lim[1], alpha.f = 0.3))
            }
            ## If two limits should be plotted, overlay one ploygon on top of
            ## the other
            if (plotlimit == TRUE & length(limit) == 2) {
                polygon(x = c(0, max(xlims),
                              max(xlims), 0),
                        y = c(0, 0, min(trafo(limit[1]), ylims[2]),
                              min(trafo(limit[1]), ylims[2])),
                        col = adjustcolor(col.lim[1], alpha.f = 0.3),
                        border = adjustcolor(col.lim[1], alpha.f = 0.3))
                polygon(x = c(0, max(xlims),
                              max(xlims), 0),
                        y = c(0, 0, min(trafo(limit[2]), ylims[2]),
                              min(trafo(limit[2]), ylims[2])),
                        col = adjustcolor(col.lim[2], alpha.f = 1),
                        border = adjustcolor(col.lim[2], alpha.f = 1))
            }
            box(bty = "l")
            rug(xlims[1]:(xlims[2] - 1), ticksize = -0.01)

            ## If there are years with all values < LOD. Draw a bar
            if (any(pldat$aggdata$all.lod == 1)) {
                bardat <- subset(pldat$aggdata, all.lod == 1)
                for (i in 1:nrow(bardat)) {
                    rect(xleft = bardat$YEAR[i] - 0.25,
                         ybottom = 0,
                         xright = bardat$YEAR[i] + 0.25,
                         ytop = trafo(bardat[[var]][i] * sqrt(2)),
                         col = col.rect,
                         border = "transparent")
                }
            }
            ## Add the points, first the raw data
            points(pldat$data$YEAR, trafo(pldat$data[[var]]), pch = pch.data,
                   cex = cex.data, col = col.data)
            ## And the aggregated data
            points(pldat$aggdata$YEAR, trafo(pldat$aggdata[[var]]),
                   pch = pch.point, cex = cex.point,
                   col = col.point)
            ## If there are outliers, mark them with a + sign
            if (!is.null(pldat$linmod)) {
                if (any(pldat$aggdata$out == 1, na.rm = TRUE) & !is.null(outlier)) {
                    do <- subset(pldat$aggdata, out == 1)
                    text(do$YEAR, trafo(do[[var]]), "+", cex = 1.5,
                         font = 2, col = col.line[1])
                }}
            ## Add confidence intervals if applicable
            segments(x0 = pldat$aggdata$YEAR,
                     y0 = ifelse(pldat$aggdata$n > nobs,
                                 trafo(lower), 0),
                     x1 = pldat$aggdata$YEAR,
                     y1 = ifelse(pldat$aggdata$n > nobs,
                                 trafo(upper), 0),
                     col = col.point)
            ## Add the regression line
            if (!is.null(pldat$linmod)) {
                if (p.line < pval.line[1] & plotline == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$linmod$fitted),
                          col = col.line[1],
                          lwd = lwd.line)
                }
                if (p.line >= pval.line[1] & p.line < pval.line[2] &
                    plotline == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$linmod$fitted),
                          col = col.line[2],
                          lwd = lwd.line, lty = 1)
                }
                if (p.line >= pval.line[2] & force == TRUE &
                    plotline == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$linmod$fitted),
                          col = col.line[2],
                          lwd = lwd.line, lty = 2)
                }
                ## Regression line last 10 years
                if (!is.null(pldat$linmod10)) {
                    if (p.line10 < pval.line10[1] & plotline10 == TRUE) {
                        lines(x = pldat$linmod10$years,
                              y = trafo(pldat$linmod10$fitted),
                              col = col.line[1],
                              lwd = lwd.line)
                    }
                }
                if (!is.null(pldat$linmod10)) {
                    if (p.line10 >= pval.line10[1] & p.line10 < pval.line10[2] &
                        plotline10 == TRUE) {
                        lines(x = pldat$linmod10$years,
                              y = trafo(pldat$linmod10$fitted),
                              col = col.line[2],
                              lwd = lwd.line, lty = 1)
                    }
                }
                if (!is.null(pldat$linmod10)) {
                    if (p.line10 >= pval.line10[2] & force == TRUE &
                        plotline10 == TRUE) {
                        lines(x = pldat$linmod10$years,
                              y = trafo(pldat$linmod10$fitted),
                              col = col.line[2],
                              lwd = lwd.line, lty = 2)
                    }
                }
                ## Smooth
                if (p.sm < pval.sm[1] & plotsmooth == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$smooth$fitted),
                          col = col.sm[1],
                          lwd = lwd.sm)
                }
                if (p.sm >= pval.sm[1] & p.sm < pval.sm[2] & plotsmooth == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$smooth$fitted),
                          col = col.sm[2],
                          lwd = lwd.sm, lty = 1)
                }
                if (p.sm >= pval.sm[2] & force == TRUE & plotsmooth == TRUE) {
                    lines(x = pldat$aggdata$YEAR,
                          y = trafo(pldat$smooth$fitted),
                          col = col.sm[2],
                          lwd = lwd.sm, lty = 2)
                }
                ## Changepoint if applicable
                if (!is.null(pldat$changepoint) & plot.changep == TRUE) {
                    seg1 <- which(pldat$aggdata$YEAR <=
                                  pldat$changepoint$changepoint)
                    seg2 <- which(pldat$aggdata$YEAR >
                                  pldat$changepoint$changepoint)
                    lines(x = pldat$aggdata$YEAR[seg1],
                          y = trafo(pldat$changepoint$fitted[seg1]),
                          col = col.line[1],
                          lwd = lwd.line)
                    lines(x = pldat$aggdata$YEAR[seg2],
                          y = trafo(pldat$changepoint$fitted[seg2]),
                          col = col.line[1],
                          lwd = lwd.line)
                }
                ## Add a star (*) if there are data values above the upper plot limit
                if (max(trafo(pldat$data[[var]]), na.rm = TRUE) > ylims[2]) {
                    yrs <- unique(subset(pldat$data, trafo(pldat$data[[var]]) > ylims[2],
                                         select = YEAR)$YEAR)
                    text("*", x = yrs, y = 0, pos = 3)
                }
                ## Set the figure text
                slopetext <- paste0(sprintf("%2.2f", pldat$linmod$slope), "% (",
                                    sprintf("%2.2f", pldat$linmod$lower, 1), ", ",
                                    sprintf("%2.2f", pldat$linmod$upper, 1), ")")

                r2 <- sprintf("%2.2f", pldat$linmod$r2)
                r2text <- bquote(R^2 == .(r2))
                if(p.line < 0.001) {
                    ptext <- "p < 0.001"
                } else ptext <- bquote(p == .(sprintf("%1.3f", p.line)))
                                
                lastyear <- max(pldat$aggdata$YEAR)
                if (p.sm < p.line) {
                    est <- sprintf("%2.2f", trafo(pldat$smooth$yhat.last), 1)
                    lwr <- sprintf("%2.2f", trafo(pldat$smooth$yhat.last.lower), 1)
                    upr <- sprintf("%2.2f", trafo(pldat$smooth$yhat.last.upper), 1)
                    yhattext <- bquote(y(.(lastyear)) ==
                                       .(est) ~ (.(lwr)*","~.(upr)))
                } else {
                    est <- sprintf("%2.2f", trafo(pldat$linmod$yhat.last), 1)
                    lwr <- sprintf("%2.2f", trafo(pldat$linmod$yhat.last.lower), 1)
                    upr <- sprintf("%2.2f", trafo(pldat$linmod$yhat.last.upper), 1)
                    yhattext <- bquote(y(.(lastyear)) ==
                                       .(est) ~ (.(lwr)*","~.(upr)))
                }
                if (!is.na(limit[1]) & !is.na(fat) & is.na(newlimit)) {
                    ## ltext <- sprintf("%2.0f", limit)
                    ## ftext <- sprintf("%2.0f", fat)
                    if (length(limit) == 1) {
                        ltext <- signif(limit, ifelse(limit < 0.01, 1, 2))
                        ftext <- signif(fat, 2)

                        if ("met" %in% pldat$stattype) {
                            limittext <- bquote(Threshold == .(ltext)*","~dw == .(ftext)*"%")
                        } else {
                            limittext <- bquote(Threshold == .(ltext)*","~lw == .(ftext)*"%")
                        }}
                    if (length(limit) == 2) {
                        ltext1 <- signif(limit[1], ifelse(limit[1] < 0.01, 1, 2))
                        ltext2 <- signif(limit[2], ifelse(limit[2] < 0.01, 1, 2))
                        ftext <- signif(fat, 2)
                        limittext1 <- bquote(Threshold == .(ltext1)*","~lw == .(ftext)*"%")
                        limittext2 <- bquote(Threshold == .(ltext2)*","~lw == .(ftext)*"%")
                    }
                }
                if (!is.na(newlimit) & is.na(fat)) {
                        ltext <- signif(limit, ifelse(limit < 0.01, 1, 2))
                        ftext <- ""
                }
                if (statstext == TRUE) {
                    text(bquote(Slope == .(slopetext)),
                         x = grconvertX(xpos, "npc"),
                         y = grconvertY(ypos, "npc"),
                         pos = 4, cex = cex.text,
                         adj = c(0, 0.5))
                    
                    text(bquote(paste(.(r2text), ", ", .(ptext))),
                         x = grconvertX(xpos, "npc"),
                         y = grconvertY(ypos - 0.04, "npc"), pos = 4,
                         adj = c(0, 0.5), cex = cex.text)

                    text(yhattext, x = grconvertX(xpos, "npc"),
                         y = grconvertY(ypos - 0.08, "npc"),
                         pos = 4, adj = c(0, 0.5), cex = cex.text)
                    if (exists("limittext")) {
                        text(limittext, x = grconvertX(xpos, "npc"),
                             y = grconvertY(ypos - 0.12, "npc"),
                             pos = 4, adj = c(0, 0.5), cex = cex.text)
                    }
                    if (exists("limittext1") & exists("limittext2")) {
                        text(limittext1, x = grconvertX(xpos, "npc"),
                             y = grconvertY(ypos - 0.12, "npc"),
                             pos = 4, adj = c(0, 0.5), cex = cex.text)
                        text(limittext2, x = grconvertX(xpos, "npc"),
                             y = grconvertY(ypos - 0.16, "npc"),
                             pos = 4, adj = c(0, 0.5), cex = cex.text)
                    }}
            }
            ## Get the locale name
            type <- pldat$stattype[1]
            type <- ifelse(type == "uria", "hav", type)
            loc <- getElement(stats[[type]], as.character(unique(pldat$data$LOC)))$name
            spec <- getElement(specs,
                               as.character(unique(pldat$data$GENUS)))$name
            ## Add age subset if applicable
            if (unique(pldat$aggdata$as) == 1) {
                titletext <- paste0(loc, " (", min(pldat$data$ALDR, na.rm = TRUE), "-",
                                    max(pldat$data$ALDR, na.rm = TRUE), "), ",
                                    spec)
            } else {
                titletext <- paste0(loc, ", ", spec)    
            }
            mtext(titletext, line = 1, 
                  at = ifelse(is.null(mtext.at), par("usr")[1], mtext.at),
                  cex = cex.mtext, font = 2, adj = 0, padj = 1)
        } else NULL
    }
    ## If everything fails, just return NULL
    else NULL
}
