# This function is a wrapper for optimizer functions that allows the user to use
# stats::optim, ucminf::ucminf, or optimParallel::optimParallel
get.eha.optim.fun <- function(method = getOption("eha.optim.method")) {
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
        if ((list(...)$control$trace %||% 0) > 0) cat("ucminf message:", ans$message, "\n")
        ans
      }
    },
    \(...) stats::optim(..., method = method)
  )
}
