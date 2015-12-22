#' @export
#' @rdname metrics
freq <- function(x, stratum) {
  if (missing(stratum)) {
    n <- 2 * nrow(x)
    nloc <- dim(x)[2]
    xx <- lapply(1:nloc, function(i) table(x[, i, ])/n)
    names(xx) <- colnames(x)
    if (is.null(names(xx))) names(xx) <- paste0("L", 1:length(xx))
    xx <- do.call(rbind, lapply(names(xx), function(y) data.frame(
      locus = y,
      xx[[y]])))
    names(xx)[2:3] <- c("allele", "freq")
    xx
  } else {
    res <- lapply(split(1:nrow(x), stratum), function(i, g) freq(g[i, , ]), g = x)
    res_stratum <- rep(names(res), sapply(res, nrow))
    res <- do.call(rbind, res)
    res$stratum <- res_stratum
    res
  }
}
