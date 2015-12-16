#' @export
#' @rdname structure
dest <- function(x, stratum, size.correct = TRUE) {

  if (missing(stratum)) {
    stop("stratum needed")
  }

  if (FALSE) {
    x <- dat_fp
    stratum <- rownames(dat_fp)
    size.correct = TRUE
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

    c(dest = (ht - hs) / (1 - hs) * (k / (k - 1)), hs = hs, ht = ht)


  })


  g_hs <- mean(res[2, ], na.rm = TRUE)
  g_ht <- mean(res[3, ], na.rm = TRUE)


  list(per_locus = res[1, ], global = (g_ht - g_hs) / (1 - g_hs) * (k / (k - 1)))
}