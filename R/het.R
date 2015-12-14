#' @export
#' @rdname metrics
het_obs <- function(x, stratum) {
  if (missing(stratum)) {
    bb1 <- x[, , 1] != x[, , 2]
    colSums(bb1, na.rm = TRUE) / nrow(x)
  } else {
    res <- t(sapply(split(1:nrow(x), stratum), function(i, g) het_obs(g[i, , ]), g = x))
    colnames(res) <- colnames(x)
    res
  }
}

#' @export
#' @rdname metrics
het_exp <- function(x, stratum) {
  if (missing(stratum)) {
    nL <- nrow(x) * 2
    n <- 2 * nrow(x)
    nloc <- dim(x)[2]
    1 - sapply(1:nloc, function(i) sum((tabulate(x[, i, ])/n)^2))
  } else {
    res <- t(sapply(split(1:nrow(x), stratum), function(i, g) het_exp(g[i, , ]), g = x))
    colnames(res) <- colnames(x)
    res
  }
}




