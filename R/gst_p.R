#' @export
#' @rdname structure


gst_p <- function(x, stratum, size.correct = TRUE) {

  if (missing(stratum)) {
    stop("stratum needed")
  }

  if (!is.factor(stratum)) {
    stratum <- factor(stratum)
  }

  stratum <- droplevels(stratum)
  k <- length(levels(stratum))

  if (size.correct) hmean <- harmonic_mean(stratum)

  res <- sapply(1:ncol(x), function(i) {

    hs <- hs(x[, i, ], stratum)
    ht <- ht(x[, i, ])

    if (size.correct) {
      hs <- size_correct_hs(hs, hmean)
      ht <- size_correct_ht(ht, hs, k, hmean)
    }

    gst_prime <- k * (ht - hs) / ((k * ht - hs) * (1 - hs))

   #   gst_prime <- (1 - hs / ht) * (k - 1 + hs)
   #   gst_prime <- gst_prime / ((k - 1) * (1 - hs))


    c(gst_p = gst_prime, hs = hs, ht = ht)
  })

  g_hs <- mean(res[2, ], na.rm = TRUE)
  g_ht <- mean(res[3, ], na.rm = TRUE)

  list(per_locus = res[1, ],
       global = k * (g_ht - g_hs) / ((k * g_ht - g_hs) * (1 - g_hs)))
}



gst_p1 <- function(x, stratum, size.correct = FALSE) {

  k <- length(unique(stratum))

  res <- sapply(1:ncol(x), function(i) {

    bb1 <- unique(as.vector(x[, i, ]))
    pv <- pvec(x[, i, ,drop = FALSE], bb1)

    ht <- 1 - sum((pv / sum(pv))^2, na.rm = TRUE)

    hs <- mean(na.rm = TRUE, sapply(split(1:nrow(x), stratum), function(k) {
      s <- pvec(x[k, i, , drop = FALSE], bb1)
      1 - sum((s/sum(s))^2)
    }) )

    if (size.correct) {
      n.harmonic <- 1/mean(1/table(stratum))
      hs.estimated <- (2*n.harmonic)/(2*n.harmonic -1) * hs
      ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
      Gst_prime <- ((1-hs.estimated/ht.estimated)*(k-1+hs.estimated) )
      Gst_prime <- Gst_prime / ((k-1)*(1-hs.estimated))
    } else {
      Gst_prime <- (1-hs/ht) * (k-1+hs)
      Gst_prime <- Gst_prime / ((k-1)*(1-hs))
    }

    c(gstp = Gst_prime, hs = hs, ht = ht)
  })

  if (ncol(res) > 1) {
    Hs.tot <- 1.0 / mean( 1/res[2, ] ,na.rm=TRUE)
    Ht.tot <- 1.0 / mean( 1/res[3, ] ,na.rm=TRUE)

    res[1, ][res[1, ] < 0 ] <- NA
    Gst.tot <- 1.0 / mean(1/res[1, ] ,na.rm=TRUE)
    res <- cbind(res, c(Gst.tot, Hs.tot, Ht.tot))
  }
  res
}

