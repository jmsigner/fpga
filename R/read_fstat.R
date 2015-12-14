#' Read data genetic data
#'
#' The function \code{read_fstat} reads data stored in the Fstat file format (http://heidi.chnebu.ch/doku.php?id=fstat). Typicall Fstat files have the extension \code{*.dat}.
#' @param fn Character string, the file name.
#' @return A three dimensional array to be used with the \code{fpga} package. Population names a stored as row names.
#' @name read_genetic_data
NULL


#' @export
#' @rdname read_genetic_data


read_fstat <- function(fn) {

  if (FALSE) {
    fn <- "~/201_projects/2015_fpga/easypop/simulation1001.dat"
  }

  txt <- scan(fn, what = "character", sep = "\n", quiet = TRUE)
  txt <- gsub("\t", " ", txt, perl = TRUE)
  ncode <- as.integer(unlist(strsplit(txt[1], " "))[4])
  NA.char <- sapply(1:ncode, function(i) paste(rep("0", i), collapse = ""))
  info <- as.numeric(unlist(strsplit(txt[1], "([[:space:]]+)")))
  n_loci <- info[2]
  loc_names <- txt[2:(n_loci + 1)]
  txt <- txt[-(1:(n_loci + 1))]
  txt <- stringi::stri_trim(txt)
  txt <- do.call(rbind, stringi::stri_split(txt, regex = "([[:space:]]+)|([[:blank:]]+)"))
  pop <- txt[, 1]
  txt <- txt[, -1]

  n_loci <- length(loc_names)
  n_ind <- nrow(txt)

  ind <- abind::abind(matrix(as.integer(stringi::stri_sub(txt, 1, info[4])), ncol = n_loci, nrow = n_ind),
                      matrix(as.integer(stringi::stri_sub(txt, info[4] + 1, info[4] * 2)), ncol = n_loci, nrow = n_ind),
                      along = 3)

  dimnames(ind)[1:2] <- list(pop, loc_names)
  ind
}

# read_genepop