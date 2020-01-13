##' Conversion of NKOO and EKOO to lat long.
##'
##' None.
##' @title ne2latlong
##' @param north NKOO
##' @param east EKOO
##' @return Latitude and longitude
##' @author Erik Lampa
##' @export
ne2latlong <- function(north, east) {

    ## Conversion between NKOO, EKOO and lat long
    
    a <- 6378137
    f <- 1/298.257222101
    e <- sqrt(f * (2 - f))
    n <- f/(2 - f)
    fn <- -667.711
    fe <- 1500064.274
    k0 <- 1.00000561024
    lam0 <- (15 + 48/60 + 22.624306/3600) * pi/180
    ahat <- a/(1 + n) * (1 + 1/4 * n^2 + 1/64 * n^4)
    v1 <- (north - fn)/(k0 * ahat)
    v2 <- (east - fe)/(k0 * ahat)

    d1 <- 1/2 * n - 2/3 * n^2 + 37/96 * n^3 - 1/360 * n^4
    d2 <- 1/48 * n^2 + 1/15 * n^3 - 437/1440 * n^4
    d3 <- 17/480 * n^3 - 37/840 * n^4
    d4 <- 4397/161280 * n^4

    v1p <- v1 - d1 * sin(2 * v1) * cosh(2 * v2) -
        d2 * sin(4 * v1) * cosh(4 * v2) -
        d3 * sin(6 * v1) * cosh(6 * v2) -
        d4 * sin(8 * v1) * cosh(8 * v2)

    v2p <- v2 - d1 * cos(2 * v1) * sinh(2 * v2) -
        d2 * cos(4 * v1) * sinh(2 * v2) -
        d3 * cos(6 * v1) * sinh(4 * v2) -
        d4 * cos(8 * v1) * sinh(8 * v2)

    phip <- asin(sin(v1p) / cosh(v2p))
    lamdel <- atan(sinh(v2p) / cos(v1p))

    long <- lam0 + lamdel

    A <- e^2 + e^4 + e^6 + e^8
    B <- -1/6 * (7 * e^4 + 17 * e^6 + 30 * e^8)
    C <- 1/120 * (224 * e^6 + 889 * e^8)
    D <- -1/1260 * 4279 * e^8

    lat <- phip + sin(phip) * cos(phip) *
        (A + B * sin(phip)^2 + C * sin(phip)^4 + D * sin(phip)^6)
    cbind(180/pi * lat, 180/pi * long)
}
