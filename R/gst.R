#' Population genetic structure
#'
#' Functions to calculate population genetic differences between populations.
#' @param x Three dimensional array with individuals, loci and alleles in the first, second and third dimension.
#' @param stratum Vector of length \code{nrow(x)} indicating population membership.
#' @param size.correct Logical value indicating whether or not to correct for sample size.
#' @name structure
NULL

pvec <- function(x, m) {
  i1 <- match(x[, 1], m)
  i2 <- match(x[, 2], m)
  tabulate(c(i1, i2))/2
}

hs <- function(x, stratum) {
  bb1 <- unique(as.vector(x))
  mean(
    sapply(split(1:nrow(x), stratum), function(i) {
      s <- pvec(x[i, ], bb1)
      1 - sum((s/sum(s))^2)
    }
    ), na.rm = TRUE)
}

ht <- function(x) {
  bb1 <- unique(as.vector(x))
  pv <- pvec(x, bb1)
  1 - sum((pv / sum(pv))^2)
}

size_correct_hs <- function(hs, hmean) {
  (2 * hmean) / (2 * hmean - 1) * hs
}

size_correct_ht <- function(ht, hs_est, k, hmean) {
  ht + hs_est / (2 * k * hmean)
}

harmonic_mean <- function(stratum) {
  1 / mean(1 / table(stratum))
}

#' @export
#' @rdname structure
gst <- function(x, stratum, size.correct = TRUE) {
  
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
