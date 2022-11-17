#' @title simple.cc
#'
#' @description Simple cross correlation of two vectors. Uses zero embedding to find optimal overlap. Also
#' has an option to normalise by the longest vector (divides final difference by length).
#' This version returns the time difference for best overlap.
#'
#' @param s1 the first numeric vector (required)
#' @param s2 the second numeric vector (required)
#' @param norm if `TRUE` the final difference is divided by the length of the longest vector
#'
#' @return Returns an integer, which is the start of s1 relative to s2.
#' E.g., -1 means that s1 has to be moved one step back to be aligned with s2.
#' @examples
#' s1 = c(0, 0, 0, 1, 1, 2, 0)
#' s2 = c(0, 0, 2, 2, 3, 0, 0, 0, 0)
#' offset = simple.cc(s1, s2) # -1
#' index_s1 = seq(1, length(s1)) + offset # align
#' plot(s2, type = 'b')
#' points(index_s1, s1, col = 2, type = 'b')
#' @export

simple.cc = function(s1, s2, # two vectors
                     norm = F){ # if T, the final difference is divided by the length of the longest vector

  # Run through positions
  np = length(s1) + length(s2) - 1
  os = sapply(seq(1, np, 1), function(i){
    r = length(s1)
    t1 = c(rep(0, max(0, i - length(s1))),
           s1,
           rep(0, max(0, length(s2) - i)))
    t2 = c(rep(0, max(0, length(s1) - i)),
           s2,
           rep(0, max(0, i - length(s2))))
    o = sum(abs(t2 - t1))
    return(o)
  })

  # Return
  return(which(os == min(os)) - length(s1))

}
