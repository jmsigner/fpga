#' @export
#' @param FUN Function to rarify.
#' @param nperm Number of permutations.
#' @rdname metrics
#'
rarify <- function(x, FUN, stratum, nperm = 100 ) {
  if (missing(stratum)) {
    n <- nrow(x)
    replicate(nperm, FUN(x[sample.int(n, replace = TRUE), , ]))
  } else {
    lapply(split(1:nrow(x), stratum),
           function(i)
             rarify(x[i, , , drop = FALSE], FUN = FUN, nperm = nperm))
  }
}