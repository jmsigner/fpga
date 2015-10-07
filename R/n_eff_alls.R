#' @export
#' @rdname metrics
n_eff_alls <- function(x, stratum) {
  if (missing(stratum)) {
    1 / (1 - het_exp(x))
  } else {
    sapply(split(1:nrow(x), stratum), function(i, g) n_eff_alls(g[i, , ]), g = x)
  }
}
