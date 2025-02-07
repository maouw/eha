glance.aftreg <- function (x, ...)
{
  stopifnot(requireNamespace("tibble",quietly=TRUE))
  iter <- x$call$control$maxiter %||% formals(eha::aftreg)$control$maxiter %||% NA_integer_

  if (is.null(x$df)) {
    df <- sum(!is.na(coef))
  }
  else {
    df <- round(sum(x$df), 2)
  }

  dd <- diag(x$var)
  df_residual <- x$n - sum(!is.na(dd) & dd > 0)
  logtest <- -2 * (x$loglik[1] - x$loglik[2])
  tibble::tibble(
    iter = as.integer(iter),
    df = as.integer(df),
    statistic = as.double(logtest),
    logLik = as.double(stats::logLik(x)),
    AIC = as.double(stats::AIC(x)),
    BIC = as.double(stats::BIC(x)),
    df.residual = as.double(df_residual),
    nobs = as.integer(stats::nobs(x)),
    p.value = as.double(1 - pchisq(logtest, df))
  )
}

tidy.aftreg <- function (x,
                         conf.level = 0.95,
                         conf.int = FALSE,
                         ...)
{
  if ("exponentiate" %in% names(list(...))) {
    message(
      "The `exponentiate` argument is not supported in the `tidy()` method for `aftreg` objects and will be ignored."
    )
  }
  stopifnot(all(
    c("coefficients", "loglik", "linear.predictors", "ttr") %in% names(x)
  ))
  stopifnot(requireNamespace("dplyr", quietly=TRUE))
  stopifnot(requireNamespace("tibble",quietly=TRUE))

  summ <- summary(x)
  ret <- summ$coefficients |> tibble::as_tibble(rownames = 'term') |> dplyr::select(
    term,
    estimate = coef,
    std.error = `se(coef)`,
    statistic = z,
    p.value = `Wald p`
  )
  intercept_and_scale <- tibble::tibble(
    term = c("(Intercept)", "Log(scale)"),
    estimate = c(x$coefficients['log(scale)'], x$coefficients["log(shape)"]),
    std.error =  sqrt(diag(x$var[c("log(scale)", "log(shape)"), c("log(scale)", "log(shape)")]))
  ) |> dplyr::mutate(
    statistic = estimate / std.error,
    p.value = pchisq(statistic ^ 2, df = 1, lower.tail = FALSE)
  )
  ret <- dplyr::bind_rows(intercept_and_scale[1, ], ret, intercept_and_scale[2, ])
  if (conf.int) {
    ci <- tibble::as_tibble(confint(x, level = conf.level),rownames = 'term')
    names(ci) <- c("term", "conf.low", "conf.high")
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  ret
}
