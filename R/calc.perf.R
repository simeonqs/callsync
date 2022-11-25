#' @title calc.perf
#'
#' @description Calculates the performance of the detections.
#'
#' @param d data frame, detection selection table with start = start time in seconds, end = end time in
#' seconds and file = file name
#' @param gt data frame, ground truth selection table with start = start time in seconds, end = end time in
#' seconds and file = file name
#'
#' @return Returns a named list with tp = the row numbers of the true positives, fp = the row numbers of the
#' false positives, fp_rate = `length(pf)/nrow(d)` and tp_rate = `length(tp)/nrow(gt)`.
#'
#' @export

calc.perf = function(d, gt){

  # Find the tp and fp
  tp = fp = c()
  for(i in 1:nrow(d)){
    sub = gt[which(gt$file == d$file[i]),]
    if(nrow(sub) == 0) {fp = c(fp, i); next}
    # keep if the start of the detection falls within a ground truth (after start and before end)
    keep_start = sapply(1:nrow(sub), function(j) d$start[i] > sub$start[j] & d$start[i] < sub$end[j])
    # or if the end falls within detections -> either case there is some overlap
    keep_end = sapply(1:nrow(sub), function(j) d$end[i] > sub$start[j] & d$end[i] < sub$end[j])
    keep = keep_start | keep_end
    if(any(keep)) tp = c(tp, i) else fp = c(fp, i)
  }

  # Calculate rates
  fp_rate = length(fp)/nrow(d)
  tp_rate = length(tp)/nrow(gt)

  # Return
  return(list(tp = tp, fp = fp, fp_rate = fp_rate, tp_rate = tp_rate))

}
