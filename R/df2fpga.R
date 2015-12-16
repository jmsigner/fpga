#' @export
#' @rdname read_genetic_data

genind2fpga <- function(x) {
  if (!all(adegenet::ploidy(x) == 2)) {
    stop("fpga only support diploid data")
  }

  xx <- adegenet::genind2df(x, usepop = FALSE, oneColPerAll = TRUE)


  ind <- abind::abind(data.matrix(xx[, seq(1, ncol(xx), 2)]),
                      data.matrix(xx[, seq(2, ncol(xx), 2)]),
                      along = 3)

  rownames(ind) <- adegenet::pop(x)
  ind
}