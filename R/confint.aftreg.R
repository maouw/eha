#' @export
confint.aftreg <- function(object, parm, level = 0.95, ...) {
  stopifnot(all(
    c("coefficients", "loglik", "linear.predictors", "ttr") %in% names(object)
  ))
  cf <- object$coefficients
  varcoef <- diag(object$var)
  ses <- sqrt(varcoef)

  if (missing(parm)) {
    parm <- setdiff(names(ses), c("log(shape)"))
  } else if (is.numeric(parm)) {
    parm <- names(ses)[parm]
  }
  qn <- qnorm(1 - (1 - level) / 2)
  lower <- (cf - qn * ses)
  upper <- (cf + qn * ses)

  pct <- paste(format(
    100 * c((1 - level) / 2, 1 - (1 - level) / 2),
    trim = TRUE,
    scientific = FALSE,
    digits = 3
  ), "%")

  cis <- cbind(lower[parm], upper[parm])
  cis <- rbind(cis[nrow(cis), ], cis[-nrow(cis), ])
  rownames(cis) <- parm

  colnames(cis) <- pct
  cis
}
