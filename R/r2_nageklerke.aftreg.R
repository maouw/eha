#' Calculate Nagelkerke's pseudo-R2 for  \code{aftreg} object
#'
#'
#' @param object A \code{aftreg} object
#' @keywords r2 nagelkerke aftreg
#' @details Based on \code{performance::.r2_nagelkerke}
#'
#' @export
r2_nagelkerke.aftreg <- function(model, ...) {
  l_base <- model$loglik[[1]]
  L.full <- model$loglik[[2]]
  D.full <- -2 * L.full
  D.base <- -2 * l_base
  n <- nobs(L.full)
  r2_nagelkerke <- as.vector((1 - exp((D.full - D.base) / n)) / (1 - exp(-D.base /
    n)))
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}
