#' @export

hmean <- function(x, ...) {
  1 / mean(1/x, ...)
}