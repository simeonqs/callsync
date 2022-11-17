# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper
# Date started: 09-04-2021
# Date last modified: 10-05-2022
# Author: Simeon Q. Smeele
# Description: Simple cross correlation of two vectors. Uses zero embedding to find optimal overlap. Also
# has an option to normalise by the longest vector (divides final difference by length).
# This version returns the time difference for best overlap.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#' @title simple.cc
#'
#' @description Simple cross correlation of two vectors. Uses zero embedding to find optimal overlap. Also
#' has an option to normalise by the longest vector (divides final difference by length).
#' This version returns the time difference for best overlap.
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @importFrom dplyr "%>%"

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
