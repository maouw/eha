# stats::confint method for aftreg objects
#' @exportS3Method stats::confint
confint.aftreg <- function(object, parm, level = 0.95, ...) {
    stopifnot(all(c("coefficients", "loglik", "linear.predictors", "ttr") %in% names(object)))
    cf <- object$coefficients
    varcoef <- diag(object$var)
    ses <- sqrt(varcoef)

    if (missing(parm)) {
        parm <- setdiff(names(ses), c("log(shape)"))
    } else if (is.numeric(parm)) {
        parm <- names(ses)[parm]
    }
    qn <- qnorm(1 - (1 - level)/2)
    lower <- (cf - qn * ses)
    upper <- (cf + qn * ses)

    pct <- paste(format(100 * c((1 - level)/2, 1 - (1 - level)/2), trim = TRUE, scientific = FALSE, digits = 3), "%")

    cis <- cbind(lower[parm], upper[parm])
    rownames(cis) <- parm
    colnames(cis) <- pct
    cis
}

# stats::confint method for summary.aftreg objects
#' @exportS3Method stats::confint
confint.summary.aftreg <- function(object, parm, level = 0.95, ...) {
    if (missing(parm)) {
        parm <- rownames(object$coefficients)
    }
    qn <- qnorm(1 - (1 - level)/2)

    lower <- object$coefficients[, 1] - qn * object$coefficients[, 3]
    upper <- object$coefficients[, 1] + qn * object$coefficients[, 3]
    names(lower) <- rownames(object$coefficients)
    names(upper) <- rownames(object$coefficients)


    pct <- paste(format(100 * c((1 - level)/2, 1 - (1 - level)/2), trim = TRUE, scientific = FALSE, digits = 3), "%")
    cis <- cbind(lower[parm], upper[parm])
    rownames(cis) <- parm
    colnames(cis) <- pct
    cis
}
