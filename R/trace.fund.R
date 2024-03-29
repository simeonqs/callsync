#' @title trace.fund
#'
#' @description Traces the fundamental frequency from a wave object. Also
#' applies smoothening to trace.
#'
#' @details Tracing step is based on a sliding window for which the spectrum
#' is calculated. A threshold is based on the maximum y value and the first
#' frequency to cross the threshold is considered the fundamental frequency.
#' If the average hight before the fundamental is higher than `noise_factor`,
#' the detection is discarded and NA is returned for that window.
#' Smoothing step is based on `smooth.spline`. Finally, all points outside
#' `freq_lim` are reset to these limits.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param hop integer, how many samples to skip for each trace point.
#' @param wl integer, window length for the spectrum
#' @param freq_lim numeric vector of length 2, frequency in kHz between which
#' to find the fundamental
#' @param spar numeric between 0-1, for the `smooth.spline` function
#' @param noise_factor numeric, how much louder the fundamental has to be
#' than the noise to be accepted
#' @param thr numeric between 0-1, the fraction of the maximum of the spectrum
#' used to detect the fundamental
#'
#' @return Data frame with time = time in seconds, fund = fundamental
#' frequency in Hz and missing = logical indicating if the fundamental was
#' detected (`TRUE`) or interpolated (`FALSE`).
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' path_git = 'https://raw.githubusercontent.com'
#' path_repo = '/simeonqs/callsync/master/tests/testthat/files'
#' file_1 = '/wave_1.wav'
#' url_1 = paste0(path_git, path_repo, file_1)
#' local_file_1 = paste(tempdir(), file_1, sep = '/')
#' if(!file.exists(local_file_1))
#'   download.file(url_1, destfile = local_file_1, mode = 'wb',)
#' wave = readWave(local_file_1)
#' trace = trace.fund(wave)
#'
#' @export
#'
#' @importFrom seewave "spec"
#' @importFrom stats "smooth.spline"

trace.fund = function(wave,
                      hop = 5,
                      wl = 200,
                      freq_lim = c(1.1, 4),
                      spar = 0.4,
                      noise_factor = 3.5,
                      thr = 0.3){

  # Trace
  final = floor(length(wave@left) - wl - 1)
  starts = seq(1, final, hop)
  funds = sapply(starts, function(start){
    w = wave[start:(start+wl)] # select part wave
    s = seewave::spec(w, plot = FALSE) # make spectrum
    s[,2][s[,1] < freq_lim[1] | s[,1] > freq_lim[2]] =
      0 # set all outside limit to 0
    s[,2] = s[,2]/max(s[,2]) # rescale the remaining spectrum
    peak = s[,1][s[,2] > thr][1]
    cont = TRUE
    ii = which(s[,1] == peak)
    while(cont){ # climb until reach first peak
      if(s[ii+1,2] > s[ii,2]) ii = ii + 1 else {
        peak = s[ii,1]
        cont = FALSE
      }
    }
    ## Only include peak if very clear
    peak = ifelse(s[ii,2] > noise_factor * mean(s[,2][s[,2]!=0]), peak, NA)
    return(peak)
  })

  if(length(which(!is.na(funds))) < 4)
    funds = rep(1, length(funds)) # fix if all points are missing

  # Smoothen trace
  d = data.frame(starts, funds)
  smoo = with(d[!is.na(d$funds),], smooth.spline(starts, funds, spar = spar))
  new_trace = with(d, predict(smoo, starts))$y

  # Move all points within limits
  new_trace[new_trace < freq_lim[1]] = freq_lim[1]
  new_trace[new_trace > freq_lim[2]] = freq_lim[2]

  # Return
  out = data.frame(time = (starts + wl/2) / wave@samp.rate,
                   fund = new_trace * 1000,
                   missing = ifelse(is.na(funds), TRUE, FALSE))
  if(all(funds == 1)) out$missing = rep(TRUE, length(out$missing))
  return(out)

}
