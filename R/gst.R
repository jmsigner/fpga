#' Population genetic structure
#'
#' Functions to population genetic differences between populations.
#' @param x Three dimensional array with individuals, loci and alleles in the first, second and third dimension.
#' @param stratum Vector of length \code{nrow(x)} indicating population membership.
#' @param size.correct Logical value indicating whether or not to correct for sample size.
#' @name structure
NULL

pvec <- function(x, m) {
  i1 <- match(x[, , 1], m)
  i2 <- match(x[, , 2], m)
  tabulate(c(i1, i2))/2
}

#' @export
#' @rdname structure
gst <- function(x, stratum, size.correct = FALSE) {

  if (!is.factor(stratum)) {
    stratum <- factor(stratum)
  }
  stratum <- droplevels(stratum)
  k <- length(levels(stratum))

  res <- sapply(1:ncol(x), function(i) {

    bb1 <- unique(as.vector(x[, i, ]))
    pv <- pvec(x[, i, ,drop = FALSE], bb1)

    ht <- 1 - sum((pv / sum(pv))^2)

    hs <- mean(na.rm = TRUE, sapply(split(1:nrow(x), stratum), function(k) {
      s <- pvec(x[k, i, , drop = FALSE], bb1)
      1 - sum((s/sum(s))^2)
    }) )
    if( ht == 0 ) {
      gst <- NA
      ht.estimated <- 0
      hs.estimated <- 0
    } else if(size.correct) {
      n.harmonic <- 1 / mean(1/table(stratum))
      hs.estimated <- (2 * n.harmonic) / (2 * n.harmonic -1) * hs
      ht.estimated <- ht + hs.estimated/(2 * k * n.harmonic)
      gst <- 1 - hs.estimated / ht.estimated
    } else {
      gst <- 1-hs/ht
      hs.estimated <- hs
      ht.estimated <- ht
    }
    c(gst = gst, hs = hs.estimated, ht = ht.estimated)
  })

  if (ncol(res) > 1) {

      Hs.tot <- sum(res[2, ], na.rm=TRUE )
      Ht.tot <- sum(res[3, ], na.rm=TRUE )
      Gst.tot <- 1 - Hs.tot / Ht.tot
      res <- cbind(res, c(Gst.tot, Hs.tot, Ht.tot))
  }
  res
}
