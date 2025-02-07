#' @export
vcov.aftreg <- function(object, complete = TRUE, ...) {
  if (!complete && any(is.na(coef(object)))) {
    keep <- !is.na(coef(object))
    vv <- object$var[keep, keep, drop = FALSE]
    vname <- names(coef(object))[keep]
  } else {
    vv <- object$var
    vname <- names(coef(object))
  }
  dimnames(vv) <- list(vname, vname)
  vv
}
