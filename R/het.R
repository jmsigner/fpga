#' @export
#' @rdname metrics
het_obs <- function(x, stratum) {
  if (missing(stratum)) {
    bb1 <- x[, , 1] != x[, , 2]
    colSums(bb1, na.rm = TRUE) / nrow(x)
  } else {
    sapply(split(1:nrow(x), stratum), function(i, g) het_obs(g[i, , ]), g = x)
  }
}

#' @export
#' @rdname metrics
het_exp <- function(x, stratum) {
  if (missing(stratum)) {
    nL <- nrow(x) * 2
    1 - apply(x, 2, function(y) sum((tabulate(y)/nL)^2))
  } else {
    sapply(split(1:nrow(x), stratum), function(i, g) het_exp(g[i, , ]), g = x)
  }
}
