#' @export
#' @rdname structure

dest <- function(x, stratum, size.correct = FALSE) {

  if (!is.factor(stratum)) {
    stratum <- factor(stratum)
  }
  stratum <- droplevels(stratum)
  k <- length(levels(stratum))

  res <- sapply(1:ncol(x), function(i) {

    bb1 <- unique(as.vector(x[, i, ]))
    pv <- pvec(x[, i, , drop = FALSE], bb1)

    ht <- 1 - sum((pv / sum(pv))^2)

    hs <- mean(na.rm = TRUE, sapply(split(1:nrow(x), stratum), function(j) {
      s <- pvec(x[j, i, , drop = FALSE], bb1)
      1 - sum((s/sum(s))^2)
    }) )


    D <- if(size.correct) {
      n.harmonic <- 1 / mean(1 / table(stratum))
      hs.estimated <- (2 * n.harmonic)/(2 * n.harmonic -1) * hs
      ht.estimated <- ht + hs.estimated/(2 * k * n.harmonic)
      (ht.estimated-hs.estimated) / (1-hs.estimated)
    } else {
      (ht-hs) / (1-hs)
    }

    D <- D / (k/(k-1))
    c(Dest = D, Hs = hs, Ht = ht)
  })
  if (ncol(res) > 1) {
    Hs.tot <- 1.0 / mean(res[2, ] ,na.rm=TRUE)
    Ht.tot <- 1.0 / mean(res[3, ] ,na.rm=TRUE)
    Dest.tot <- 1.0 / (mean(1/res[1, ], na.rm=TRUE))
    res <- cbind(res, c(Dest.tot, Hs.tot, Ht.tot))
  }
  res
}
