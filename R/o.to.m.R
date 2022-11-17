# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: chapter II 
# Date started: 26-08-2021
# Date last modified: 26-08-2021
# Author: Simeon Q. Smeele
# Description: Traces the fundamental frequency from a wave object. Also applies smoothening to trace.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

o.to.m = function(o, n){
  
  m = matrix(nrow = length(n), ncol = length(n))
  m[lower.tri(m)] = o
  md = as.dist(m)
  m[upper.tri(m)] = t(m)[upper.tri(m)]
  diag(m) = 0
  rownames(m) = colnames(m) = n
  return(m)
  
}

