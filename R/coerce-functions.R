#' @export
#' @rdname read_genetic_data
#'
genind2fpga <- function(x) {
  if (!all(adegenet::ploidy(x) == 2)) {
    stop("fpga only supports diploid data")
  }
  xx <- adegenet::genind2df(x, usepop = FALSE, oneColPerAll = TRUE)

  ind <- abind::abind(data.matrix(xx[, seq(1, ncol(xx), 2)]),
                      data.matrix(xx[, seq(2, ncol(xx), 2)]),
                      along = 3)

  rownames(ind) <- adegenet::pop(x)
  ind
}


gstudio2fpga <- function(x, pop) {
  # if (!all(gstudio::ploidy(x) == 2)) {
  #   stop("fpga only supports diploid data")
  # }
  # data for fpga
  x <- lapply(gstudio::column_class(x, class = "locus"),
              function(i) apply(gstudio::alleles(dat0[[i]]), 2, as.integer))
  x <- abind::abind(list(sapply(x, function(y) y[, 1]),
                         sapply(x, function(y) y[, 2])), along = 3)
  if (!missing(pop)) {
    rownames(x) <- pop
  }

  x
}

fpga2gstudio <- function(x) {
  y <- data.frame(apply(x, 1:2, paste0, collapse = ":"))
  for (i in 1:ncol(y)) {
    y[, i] <- gstudio::locus(y[, i], type = "separated")
  }
  if (!is.null(rownames(x))) y$pop <- rownames(x)
  y
}