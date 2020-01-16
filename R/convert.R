##' Converts the contaminant variables to account for lab changes
##'
##' For metals and DDE. For internal use only.
##' @title convert
##' @param data A data frame
##' @param inds Variable name(s) containing the contaminants
##' @param what 
##' @param genus Species
##' @param srt Class of contaminants, taken from the file name
##' @param loc Locale
##' @param co_met A list with conversion rules for metals, see \code{convert_met}
##' @param co_dde A list with conversion rules for DDE, see \code{convert_dde}
##' @param type Type of data (marine or limnic), Taken from the file name
##' @return The converted variable
##' @author Erik Lampa
##' @export
convert <- function(data, inds, what, genus, srt, loc, co_met, co_dde, type) {
    ## Performs the lab conversions
    if (srt == "met" & length(inds) > 1) {
        ## Which variable should be converted? The one with the longest name
        which.conv <- which(nchar(names(data[, inds])) == min(nchar(names(data[,
                                                                               inds]))))
        other <- which(nchar(names(data[, inds])) == max(nchar(names(data[,
                                                                               inds]))))
        d <- data[,inds]
        ## Convert to numeric, set -9 to NA
        d[] <- lapply(d[], function(x) {
            x <- as.numeric(x)
            x <- ifelse(x == -9, NA, x)
           # x <- ifelse(x < 0, abs(x)/sqrt(2), x)
            x
        })
        ## Get the denominator and perform the conversion
        den <- getElement(getElement(co_met, what), genus)
        d[,which.conv] <- d[,which.conv]/ifelse(is.null(den), 1, den)
        da <- ifelse(is.na(d[,other]), d[,which.conv], d[,other])
    }
    if (srt == "clc" & length(inds) > 1) {
        ## Similar to metals
        which.conv <- which(nchar(names(data[, inds])) ==
                            min(nchar(names(data[, inds]))))
        other <- which(nchar(names(data[, inds])) ==
                       max(nchar(names(data[,inds]))))
        d <- data[,inds]
        d[] <- lapply(d[], function(x) {
            x <- as.numeric(x)
            x
        })
        ## But a different conversion
        frac <- getElement(getElement(co_dde, genus), loc)
        d[,which.conv] <- ifelse(is.null(frac), 1, frac) * d[,which.conv]
        d[,which.conv] <- ifelse(!is.na(d[,other]),
                                 d[,other],
                                 d[,which.conv])
        da <- d[,which.conv]
    }
    if (length(inds) == 1) da <- data[,inds]
    ## Don't do anythin for limnic data
    if (type == "limn" & length(inds) == 1) {
        da <- data[,inds]
        da <- as.numeric(da)
        da <- ifelse(da == -9, NA, da)
        da
    }
    da
}
