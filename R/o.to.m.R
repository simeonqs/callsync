#' @title o.to.m
#'
#' @description Transforms a vector into a matrix where it assumes that the vector values are the lower
#' triangular of the matrix: `m[lower.tri(m)] = o`. It includes 0 on the diagonal.
#'
#' @param o the vector containing the values for the lower triangular (required)
#' @param n the names for the rows and columns of the matrix (optional)
#'
#' @return Returns a matrix where it assumes that `m[lower.tri(m)] = o`.
#'
#' @examples
#' m = matrix(1:9, nrow = 3, ncol = 3)
#' o = m[lower.tri(m)]
#' m_new = o.to.m(o)
#'
#' @importFrom stats "as.dist"
#'
#' @export

o.to.m = function(o,
                  n = seq(sqrt(length(o)+1)+1)){

  m = matrix(nrow = length(n), ncol = length(n))
  m[lower.tri(m)] = o
  md = as.dist(m)
  m[upper.tri(m)] = t(m)[upper.tri(m)]
  diag(m) = 0
  rownames(m) = colnames(m) = n
  return(m)

}

