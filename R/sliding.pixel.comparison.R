#' @title sliding.pixel.comparison
#'
#' @description Can be used to run spectrographic cross correlation. Both spectrograms are zero-padded
#' and slid over each other. For each step the difference is computed. The function returns the absolute
#' difference at the point at the minimum (maximal signal overlap).
#'
#' @param s1 numeric matrix, the first spectrogram.
#' @param s2 numeric matrix, the second spectrogram.
#' @param step_size numeric, how many pixels should be moved for each step. Default is `1`.
#'
#' @return Returns the distance at the point of maximal signal overlap.
#'
#' @export

sliding.pixel.comparison = function(s1,
                                    s2,
                                    step_size = 1){

  # Run through positions
  np = ncol(s1) + ncol(s2) - 1
  os = sapply(seq(1, np, step_size), function(i){
    m = min(s1, s2)
    r = nrow(s1)
    t1 = cbind(matrix(m, ncol = max(0, i - ncol(s1)), nrow = r),
               s1,
               matrix(m, ncol = max(0, ncol(s2) - i), nrow = r))
    t2 = cbind(matrix(m, ncol = max(0, ncol(s1) - i), nrow = r),
               s2,
               matrix(m, ncol = max(0, i - ncol(s2)), nrow = r))
    o = sum(abs(t2 - t1))
    return(o)
  })

  return(min(os))

} # end sliding.pixel.comparison
