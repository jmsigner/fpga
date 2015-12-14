#' @export
#' @param x A three dimensional array containing the genetic data.
#' @param loc_sep Character, the separator to devide alleles.
#' @param sep Character, the field separator.
#' @rdname read_genetic_data

write_genetic <- function(x, fn, loc_sep = ":", sep = ",") {
  dat <- apply(x, 1:2, paste, collapse = loc_sep)
  if (!is.null(rownames(x))) {
    dat <- cbind(pop = rownames(x), dat)
  }
  write.table(dat, file = fn, sep = sep, row.names = FALSE)
}