get.eha.optim.fun <- function(method = getOption("eha.optim.method", default = "BFGS"),
                              use.optimx = getOption("eha.optim.use.optimx", default = FALSE)) {
  use.optimx <- startsWith(method, "optimx.") || isTRUE(use.optimx)
  if (use.optimx) {
    stopifnot("Missing package 'optimx'" = requireNamespace("optimx", quietly = TRUE))
    return(function(...) {
      optimx::optimr(..., method = sub("^optimx[.]", "", method))
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