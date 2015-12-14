#' Population genetic metrics
#'
#' Functions to calculate population genetic metrics. For all functions \code{stratum} splits the data set into different populations. If \code{stratum} is missing, one population is assumed. \code{n_alls} returns the number of alleles per population. \code{rarify} rarifies a population genetic metric.
#' @param x Three dimensional array with individuals, loci and alleles in the first, second and third dimension.
#' @param stratum Vector of length \code{nrow(x)} indicating population membership.
#' @name metrics
NULL


#' @export
#' @rdname metrics
n_alls <- function(x, stratum) {
  if (missing(stratum)) {
    apply(x, 2, function(y) length(unique(as.vector(na.omit(y)))))
  } else {
    res <- t(sapply(split(1:nrow(x), stratum), function(i, g) n_alls(g[i, , ]), g = x))
    colnames(res) <- colnames(x)
    res
  }
}

