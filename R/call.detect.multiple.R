#' @title call.detect.multiple
#'
#' @description Detects multiple calls in a wave object using an amplitude envelope.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param threshold rector of length 1 or 2. The fraction of the maximum of the normalised envelope to use as
#' threshold to detect start and end. If a vector of length 2 is supplied, the first is used to detect the
#' start and the second to detect the end (in case of echo).
#' @param msmooth used as argument for the `seewave::env` function. *A vector of length 2 to smooth the
#' amplitude envelope with a mean sliding window. The first component is the window length (in number of
#' points). The second component is the overlap between successive windows (in \%).* Default is `c(500, 95)`.
#' @param plot_it  logical, if `TRUE`, returns three-panel plot of wave form, envelope and spectrogram to
#' current plotting window. Default is `FALSE`.
#' @param min_dur numeric, the minimal duration in seconds for a detection to be saved. Default is `0.1`.
#' @param max_dur numeric, the maximal duration in seconds for a detection to be saved. Default is `0.3`.
#'
#' @return Returns a data frame with start = start time in samples and end = end time in samples for each
#' detection. Optionally also plots the wave form and detections to current window.
#'
#' @export
#'
#' @importFrom seewave "env"

call.detect.multiple = function(wave,
                                threshold = 0.3,
                                msmooth = c(500, 95),
                                plot_it = F,
                                min_dur = 0.1,
                                max_dur = 0.3){

  # Envelope
  env = env(wave, msmooth = msmooth, plot = F)
  env = ( env - min(env) ) / max( env - min(env) )
  duration = length(wave@left)/wave@samp.rate

  # Find calls
  which_above = which(env > threshold)
  start = which_above[1]
  end = which_above[length(which_above)]
  other_starts = which_above[which(diff(which_above) != 1)+1]
  other_ends = which_above[which(diff(which_above) != 1)]
  starts = sort(c(start, other_starts))
  ends = sort(c(end, other_ends))

  # Remove short durations
  r = c()
  if(length(starts) > 1){
    for(j in 1:length(starts)){
      dif = ends[j] - starts[j]
      if(dif * duration/length(env) < min_dur) r = c(r, j)
    }
    if(length(r) > 0){
      starts = starts[-r]
      ends = ends[-r]
    }
  }

  # Fix time
  if(length(starts) != 0){
    starts = round((starts-1) * duration/length(env) * wave@samp.rate)
    ends = round((ends-1) * duration/length(env) * wave@samp.rate)
  }

  # Return
  detections = data.frame(start = starts, end = ends)
  return(detections)

}
