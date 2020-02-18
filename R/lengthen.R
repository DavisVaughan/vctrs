#' @export
vec_lengthen <- function(x, ptype = NULL) {
  .Call(vctrs_lengthen, x, ptype)
}
