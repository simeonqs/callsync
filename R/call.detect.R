#' @title call.detect
#'
#' @description Detects calls in a wave object using an amplitude envelope.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param threshold rector of length 1 or 2. The fraction of the maximum of the normalised envelope to use as
#' threshold to detect start and end. If a vector of length 2 is supplied, the first is used to detect the
#' start and the second to detect the end (in case of echo).
#' @param msmooth used as argument for the `seewave::env` function. *A vector of length 2 to smooth the
#' amplitude envelope with a mean sliding window. The first component is the window length (in number of
#' points). The second component is the overlap between successive windows (in \%).* Default is `c(500, 95)`.
#' @param plot_it  if `TRUE`, returns three-panel plot of wave form, envelope and spectrogram to current
#' plotting window. Default is `FALSE`.
#'
#' @return Returns a dataframe with start = start time in samples and end = end time in samples for each
#' detection. Optionally also plots the wave form and detections to current window.
#'
#' @export
#'
#' @importFrom seewave "env"
#' @importFrom graphics "abline"

call.detect = function(wave,
                       threshold = 0.3,
                       msmooth = c(500, 95),
                       plot_it = FALSE){

  # Create envelope
  env = seewave::env(wave, msmooth = msmooth, plot = F)
  env = ( env - min(env) ) / max( env - min(env) )
  duration = length(wave@left)/wave@samp.rate

  # Find max location
  where_max = which(env == 1)
  ## Left loop
  start = NA
  j = where_max
  while(is.na(start)){
    j = j - 1
    if(j == 0) start = j else if(env[j] < threshold[1]) start = j
  }
  ## Right loop
  end = NA
  j = where_max
  while(is.na(end)){
    j = j + 1
    if(j == length(env)) end = j else if(env[j] < threshold[length(threshold)]) end = j
  }

  # Re-clip wave
  start_env = start
  if(start == 0) start = 1 # avoid issues when call starts at start clip
  end_env = end
  start = round((start-1) * duration/length(env) * wave@samp.rate)
  end = round((end-1) * duration/length(env) * wave@samp.rate)
  new_wave = wave[start:end]

  # Plot
  if(plot_it){
    par(mfrow = c(2, 2))
    plot(env, type = 'l')
    abline(v = c(start_env, end_env))
    abline(h = threshold, lty = 2)
    better.spectro(wave, wl = 512, ovl = 450, ylim = c(500, 4000))
    abline(v = c(start/wave@samp.rate, end/wave@samp.rate))
    plot(wave)
    abline(v = c(start/wave@samp.rate, end/wave@samp.rate))
  }

  # Return
  detections = data.frame(start = start, end = end)
  return(detections)

}
