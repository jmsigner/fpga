#' @export
#' @rdname structure
fst <- function(x, stratum, size.correct = TRUE) {

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

    c(gst = 1 - hs / ht, hs = hs, ht = ht)
  })

  list(per_locus = res[1, ],
       global = 1 - sum(res[2, ], na.rm = TRUE) / sum(res[3, ], na.rm = TRUE))
}
