##' Function to calculate power or the minimum sample size needed to detact a
##' specified trend.
##'
##' For internal use only
##' @title pow
##' @param b The trend per year as a fraction.
##' @param n The number of years.
##' @param a The confidence level.
##' @param s2 The variance.
##' @param power The desired power.
##' @param df The degrees of freedom.
##' @return Power, minimum sample size or the lowest detectable trend.
##' @author Erik Lampa
##' @importFrom rootSolve uniroot.all
##' @export
pow <- function(b = NULL, n = NULL,
                a = NULL, s2 = NULL,
                power = NULL, df = NULL) {
    ## Main function for power calculations.
    
    powerfun <- quote({
        di <- (log(1 + b))^2 * (n - 1) * n * (n + 1)/(12 * s2)
        1 - pf(qf(a, 1, df, 0), 1, df, di) - power
    })

    if (is.null(b)) uniroot.all(function(b) eval(powerfun), c(0, 100))
    else if (is.null(n)) uniroot.all(function(n) eval(powerfun), c(3,
                                                                   100))
    else if (is.null(power)) uniroot.all(function(power) eval(powerfun),
                                         c(0, 1))
    else stop("Syntax error")
}
