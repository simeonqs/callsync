#' @title calc.perf
#'
#' @description Calculates the performance of the detections. Detections are
#' true positive if they overlap to any extend with a ground truth selection.
#'
#' @param d data frame, detection selection table with start = start time in
#' seconds, end = end time in  seconds and file = file name
#' @param gt data frame, ground truth selection table with start = start time
#' in seconds, end = end time in seconds and file = file name
#'
#' @return Returns a named list with tp = the row numbers (in d) for the true
#' positives, fp = the row numbers (in d) for the false positives, fn = the row
#' numbers (in gt) for the false negatives, fp_rate = `length(pf)/nrow(d)` and
#' tp_rate = `length(tp)/nrow(gt)`.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' file = system.file("extdata/audacity", "", package = "callsync")
#' d = load.selection.tables.audacity(path_selection_tables = file)
#' result = calc.perf(d, d)
#'
#' @export

calc.perf = function(d, gt){

  # Find the tp and fp
  tp = fp = c()
  for(i in seq_len(nrow(d))){
    sub = gt[which(gt$file == d$file[i]),]
    if(nrow(sub) == 0) {fp = c(fp, i); next}
    # keep if the start of the detection falls within a ground truth (after
    # start and before end)
    keep_start = sapply(1:nrow(sub), function(j)
      d$start[i] > sub$start[j] & d$start[i] < sub$end[j])
    # or if the end falls within detections -> either case there is some
    # overlap
    keep_end = sapply(1:nrow(sub), function(j)
      d$end[i] > sub$start[j] & d$end[i] < sub$end[j])
    # or if start and end fall around -> also overlap
    keep_around = sapply(1:nrow(sub), function(j)
      d$start[i] < sub$start[j] & d$end[i] > sub$end[j])
    keep = keep_start | keep_end | keep_around
    if(any(keep)) tp = c(tp, i) else fp = c(fp, i)
  }

  # Find the fn
  fn = c()
  for(i in seq_len(nrow(gt))){
    # fn is no detections for that file
    sub = d[which(d$file == gt$file[i]),]
    if(nrow(sub) == 0) {fn = c(fn, i); next}
    # keep if the start of the detection falls within a ground truth (after
    # start and before end)
    keep_start = sapply(1:nrow(sub), function(j)
      gt$start[i] > sub$start[j] & gt$start[i] < sub$end[j])
    # or if the end falls within detections -> either case there is some
    # overlap
    keep_end = sapply(1:nrow(sub), function(j)
      gt$end[i] > sub$start[j] & gt$end[i] < sub$end[j])
    # or if start and end fall around -> also overlap
    keep_around = sapply(1:nrow(sub), function(j)
      gt$start[i] < sub$start[j] & gt$end[i] > sub$end[j])
    keep = keep_start | keep_end | keep_around
    if(!any(keep)) fn = c(fn, i)
  }

  # Calculate rates
  fp_rate = length(fp)/nrow(d)
  tp_rate = length(tp)/nrow(gt)
  fn_rate = length(fn)/nrow(gt)

  # Return
  return(list(tp = tp,
              fp = fp,
              fn = fn,
              fp_rate = fp_rate,
              tp_rate = tp_rate,
              fn_rate = fn_rate))

}
