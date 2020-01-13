##' Tests for a possible change point in the series accoring to the methodology
##' described in \ldots
##'
##' None
##' @title do_changep
##' @param data A data frame.
##' @param y The outcome variable.
##' @param n0 The minimal number of years on each side of the change point.
##' @return A list with the change point, the value of the D statistic at the
##'     change point and the fitted values
##' @author Erik Lampa
##' @import nlme
##' @export
do_changep <- function(data, y, n0) {
    ## Tests for a changepoint in the series
    ## Set up possible changepoints. n0 is the minumum number of observations on
    ## each side of the CP
    bps <- (min(data$YEAR) + n0):(max(data$YEAR) - n0)
    dat <- data
    ## Two models, the first which allows for different slopes on either side of
    ## the CP
    fmla.b <- as.formula(paste("log(", y, ") ~ YEAR * dummy"))
    fmla.reg <- as.formula(paste("log(", y, ") ~ YEAR"))
    ## Loop over changepoints
    res <- sapply(bps, function(i) {
        ## Create a dummy variable representing the CP
        dat$dummy <- ifelse(dat$YEAR > i, 1, 0)
        ## Fit a GLS model which allows for correlated data. Maybe use lm with
        ## vcovHAC instead?
        ## First model, a model without CP
            fm0 <- try(nlme::gls(fmla.reg, data = dat,
                       correlation = corCAR1(form = ~YEAR),
                       method = "ML"), silent = TRUE)
        ## Second model, allowing for a CP
            fmb <- try(nlme::gls(fmla.b, data = dat,
                       correlation = corCAR1(form = ~YEAR),
                       method = "ML"), silent = TRUE)
        if (!inherits(fm0, "try-error") & !inherits(fmb, "try-error")) {
            ## If models have converged, calculate the logLik
                ll <- -2*(logLik(fm0) - logLik(fmb))
                as.numeric(ll)
            } else NA
    })

    if(!any(is.na(res))) {
        ## Where is the largest value of logLik?
        maxD <- max(res)
        bp <- bps[which(res == max(res))]
        n <- nrow(data)
        ## Load data from Sturludottir's thesis
        data("dcrit", envir = environment())
        if (!exists("dcrit")) stop("dcrit does not exist")
        ## Check whether any test statistic exceeds the critical value
        ind <- tail(which(dcrit$n <= n), n = 1)
        test <- ifelse(maxD > dcrit$Dcrit[ind], 1, 0)
        ## Return the result
        if(test == 1) {
            dat$dummy <- ifelse(dat$YEAR > bp, 1, 0)
            bp.fit <- lm(fmla.b, data = dat)
            list(changepoint = ifelse(length(bp) > 1, bp[1], bp),
                 D = maxD, fitted = exp(fitted(bp.fit)))
        } else NULL
    } else NULL
}
