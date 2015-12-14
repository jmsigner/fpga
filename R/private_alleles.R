#' @export
#' @rdname metrics

nprivate_alleles <- function(x, stratum) {
  w <- split(1:nrow(x), stratum)
  alls <- lapply(w, function(i, g) unique(as.vector(g[i, , ])), g = x)

  npa <- vector("numeric", length(alls))

  for (i in 1:length(alls)) {
    npa[i] <- length(setdiff(alls[[i]], do.call(c, alls[-i])))
  }
  npa
}
