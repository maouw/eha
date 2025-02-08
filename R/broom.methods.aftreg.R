# broom::glance method for aftreg objects
glance.aftreg <- function(x, ...) {
    iter <- x$call$control$maxiter %||% formals(eha::aftreg)$control$maxiter %||% NA_integer_

    if (is.null(x$df)) {
        df <- sum(!is.na(coef))
    } else {
        df <- round(sum(x$df), 2)
    }

    dd <- diag(x$var)
    df_residual <- x$n - sum(!is.na(dd) & dd > 0)
    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    tibble::tibble(iter = as.integer(iter), df = as.integer(df), statistic = as.double(logtest),
        logLik = as.double(stats::logLik(x)), AIC = as.double(stats::AIC(x)), BIC = as.double(stats::BIC(x)),
        df.residual = as.double(df_residual), nobs = as.integer(stats::nobs(x)), p.value = as.double(1 -
            pchisq(logtest, df)))
}

# broom::tidy method for aftreg.summary objects
tidy.summary.aftreg <- function(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {
    stopifnot(all(c("coefficients", "loglik", "linear.predictors", "ttr") %in% names(x)))
    ret <- tibble::as_tibble(cbind(data.frame(term = rownames(x$coefficients), stringsAsFactors = FALSE),
        as.data.frame(x$coefficients)[, c("coef", "se(coef)", "z", "Wald p")]))
    colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

    if (exponentiate) {
        ret$estimate <- exp(ret$estimate)
    }

    if (conf.int) {
        ci <- tibble::as_tibble(confint(x, level = conf.level), rownames = "term")
        names(ci) <- c("term", "conf.low", "conf.high")
        ret <- dplyr::left_join(ret, ci, by = "term")
    }
    ret
}
# broom::tidy method for aftreg objects
tidy.aftreg <- function(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {
    summ <- summary(x, use.drop1 = FALSE)
    tidy.summary.aftreg(summ, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate)
}
