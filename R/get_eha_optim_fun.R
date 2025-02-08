# This function is a wrapper for optimizer functions that allows the user to use
# stats::optim, optimx::optimr, or optimParallel::optimParallel
get.eha.optim.fun <- function(method = getOption("eha.optim.method", default = "BFGS")) {
  if (startsWith(method, "optimx.")) {
    stopifnot("Missing package 'optimx'" = requireNamespace("optimx", quietly = TRUE))
    return(function(...) {
      dots <- list(...)
      if(!is.null(dots$control$fnscale)) {
        if(as.double(dots$control$fnscale) == -1) {
          dots$control$fnscale <- NULL
          dots$control$maximize <- TRUE
        } else if (as.double(dots$control$fnscale) == 1) {
          dots$control$fnscale <- NULL
        } else {
          stop(method, " doesn't support a fnscale that isn't -1 or 1 (provided ", dots$control$fnscale)
        }
      }
      dots$method <- sub("^optimx[.]", "", method)
      do.call(optimx::optimr, dots)
    })
  }
  if (method == "optimParallel") {
    stopifnot("Missing package 'optimParallel'" = requireNamespace("optimParallel", quietly = TRUE))
    return(function(...) {
      optimParallel::optimParallel(...)
    })
  }
  method <- match.arg(method, eval(formals(stats::optim)$method))
  return(function(...) {
    optim(..., method = method)
  })
}
