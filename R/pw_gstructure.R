#' @export
#' @rdname structure

pw_gstructure <- function(x, stratum, size.correct = TRUE, what = c("gst", "gst_p", "dest")) {

  pops <- split(1:nrow(x), as.character(stratum))

#  res <- utils::combn(length(pops), 2,
#                      function (i) {
#                        match.fun(paste0("fpga::", what))(x[c(pops[[i[1]]], pops[[i[2]]]), , , drop = FALSE],
#                                        stratum[c(pops[[i[1]]], pops[[i[2]]])])$global
#                      })
#
#  attributes(res) <- list(class = "dist", Diag = FALSE, Upper = FALSE,
#                          Labels = names(pops), Size = length(pops))
#  res



  K <- length(pops)
  ret <- matrix(0,nrow=K,ncol=K)
  diag(ret) <- NA
  for( i in 1:K){
    for( j in i:K){
      if( i!=j ){
        r <- if (what == "gst") {
          gst(x[c(pops[[i]], pops[[j]]), , , drop = FALSE], stratum[c(pops[[i]], pops[[j]])],
              size.correct = size.correct)
        } else if (what == "gst_p") {
          gst_p(x[c(pops[[i]], pops[[j]]), , , drop = FALSE], stratum[c(pops[[i]], pops[[j]])],
                size.correct = size.correct)
        } else if (what == "fst") {
          fst(x[c(pops[[i]], pops[[j]]), , , drop = FALSE], stratum[c(pops[[i]], pops[[j]])],
                size.correct = size.correct)
        } else if (what == "dest") {
          dest(x[c(pops[[i]], pops[[j]]), , , drop = FALSE], stratum[c(pops[[i]], pops[[j]])],
               size.correct = size.correct)
        }
        ret[i,j] <- ret[j,i] <-  r$global
      }
    }
  }
  diag(ret) <- 0
  ret
}
