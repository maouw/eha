#' Calculate Nagelkerke's pseudo-R2 for a \code{\link[eha]{aftreg}} object
#'
#'
#' @param model A \code{\link[eha]{aftreg}} object
#' @param \dots Additional ...
#' @keywords r2 nagelkerke aftreg
#' @details Based on \code{\link[performance]{r2_nagelkerke}}
#' @examples
#' data(mort)
#' r2_nagelkerke.aftreg(aftreg(Surv(enter, exit, event) ~ ses, param = 'lifeExp', data = mort))
#' @export
r2_nagelkerke.aftreg <- function(model, ...) {
    l_base <- model$loglik[[1]]
    L.full <- model$loglik[[2]]
    D.full <- -2 * L.full
    D.base <- -2 * l_base
    n <- model$n
    r2_nagelkerke <- as.vector((1 - exp((D.full - D.base)/n))/(1 - exp(-D.base/n)))
    names(r2_nagelkerke) <- "Nagelkerke's R2"
    r2_nagelkerke
}
