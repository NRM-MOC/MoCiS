##' Fit a smooth curve to the series using LOESS with automatic smoothness
##' selection using generalized cross validation (default) or AIC to select the
##' optimal span. The smooth is tested against the linear model fit using an ANOVA.
##'
##' Defaults to a linear model if for some reason a smooth cannot be fitted.
##' @title do_smooth
##' @param data A data frame.
##' @param y The outcome variable.
##' @param alpha The significance level to be used in the test against the
##'     linear model.
##' @param crit A charater, "gcv" or "aic". The criterion for choosing the
##'     optimal span.
##' @param rob Set to TRUE to perform a robust LOESS. Very experimental!
##' @param lmod The linear model which the smooth should be compared against.
##' @return A list with elements cv containing the estimated coefficient of
##'     variation around the smooth and the lowest detectable trend the last 10
##'     years, an estimate of Kendall's tau, its p-value, the coefficient of
##'     determination, estimated contaminant level for the last year along with
##'     its confidence interval, the fitted smooth curve and the span used.
##' @author Erik Lampa
##' @import fANCOVA
##' @export
do_smooth <- function(data, y, alpha, crit, rob, lmod, n.pow, power) {
    ## Performs loess smoothing with automated bandwith selection based on "gcv"
    ## (the default) or "aic". A test is performed to assess whether the
    ## smoother fits the data better than the linear model.
    
    data <- subset(data, !is.na(data[[y]]))
    ## Set a different family if robust == TRUE
    if (rob == FALSE) fam <- "gaussian" else fam <- "symmetric"
    ## Estimate the smooth
    sm <- try(fANCOVA::loess.as(x = data[["YEAR"]],
                   y = log(data[[y]]),
                   criterion = crit,
                   family = fam),
              silent = TRUE)
    if (!inherits(sm, "try-error")) {
        ## If everything ok, extrect some statistics
        ok <- 1
        df.sm <- max(sm$enp, 1)
        df.tot <- nrow(data)
        df.res <- df.tot - df.sm
        tcrit <- qt(1 - alpha/2, df.res)
        if(sum(sm$fitted) == 0) {
            ## If everything = 0, fit a linear model and be done with it
            ok <- 0
            fmla <- as.formula(paste0("log(", y, ") ~ YEAR"))
            fit <- lm(fmla, data = data)
            fitted <- fitted(fit)
        } else fitted <- sm$fitted
        ## Calculate R^2
        sse <- sum((log(data[[y]]) - fitted)^2)
        r2 <- 1 - sse/sum(scale(log(data[[y]]), scale = FALSE)^2)
        if (ok == 1) {
            ## Now for the test...
            yy <- log(data[[y]])
            X <- model.matrix(lmod)
            H <- X %*% solve(crossprod(X)) %*% t(X)
            G <- tcrossprod(diag(nrow(H)) - H)
            L <- matrix(nrow = length(yy), ncol = length(yy))
            for(j in 1:length(yy)){
                yi = rep_len(0, length(yy))
                yi[j] = 1
                L[,j] = loess(yi~data[["YEAR"]], span = sm$pars$span)$fitted
            }
            Hs <- tcrossprod(diag(nrow(L)) - L)
            v1 <- sum(diag(G - Hs))
            v2 <- sum(diag((G - Hs)^2))
            d1 <- sum(diag(Hs))
            d2 <- sum(diag(Hs^2))
            num <- abs(t(matrix(yy)) %*% G %*% matrix(yy) -
                    t(matrix(yy)) %*% Hs %*% matrix(yy))/v1
            den <- t(matrix(yy)) %*% Hs %*% matrix(yy)/d1
            Fval <- num/den
            ## ...and finally the p-value comparing the smooth to the linear model
            pval <- pf(Fval, v1^2/v2, d1^2/d2, lower.tail = FALSE)
        } else pval <- 1
        ## Residuals
        res <- log(data[[y]]) - fitted
        ## Kendall's tau
        ken <- cor.test(x = data$YEAR, y = data[[y]], method = "kendall")
        if (sum(sm$fitted) != 0) {
            pred <- predict(sm, se = TRUE)
        } else pred <- predict(fit, se.fit = TRUE)
        i <- nrow(data)
        ## Return some statistics
        list(cv = c(100 * sqrt(exp(var(res)) - 1),
                    100 * pow(n = n.pow, a = 1 - alpha/2, s2 = var(res),
                              power = power, df = df.res)),
             tau = ken$estimate,
             p.tau = ken$p.value,
             p.smooth = pval,
             r2.smooth = r2,
             yhat.last = exp(pred$fit[i]),
             yhat.last.lower = exp(pred$fit[i] - tcrit * pred$se.fit[i]),
             yhat.last.upper = exp(pred$fit[i] + tcrit * pred$se.fit[i]),
             fitted = exp(sm$fitted),
             span = sm$pars$span)
    } else NA
}
