# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: monk parakeets
# Date started: 22-02-2021
# Date last modified: 17-03-2021
# Author: Simeon Q. Smeele
# Description: Compares two spectrograms. 
# This version includes step size. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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

} # End sliding.pixel.comparison
