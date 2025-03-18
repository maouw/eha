# This function is a wrapper for optimizer functions that allows the user to use
# stats::optim, ucminf::ucminf, or optimParallel::optimParallel
get.eha.optim.fun <- function(method = getOption("eha.optim.method", "BFGS")) {
  switch(method,
    optimParallel = {
      stopifnot("Missing package 'optimParallel'" = requireNamespace("optimParallel", quietly = TRUE))
      function(...) {
        optimParallel::optimParallel(...)
      }
    },
    ucminf = {
      stopifnot("Missing package 'ucminf'" = requireNamespace("ucminf", quietly = TRUE))
      function(..., method = NULL) {
        ans <- ucminf::ucminf(...)
        if (ans$convergence == 1 || ans$convergence == 2 || ans$convergence == 4) {
          ans$convergence <- 0
        }
        if(ans$convergence != 0) {
          warning(sprintf("ucminf did not converge (code: %s). Fitting with BFGS.", ans$convergence))
          ans <- stats::optim(..., method = "BFGS")
        }
        ans
      }
    },
    \(...) stats::optim(..., method = method)
  )
}
