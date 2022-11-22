#' @title better.spectro
#'
#' @description Creates a spectrogram and plots this to the current window.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param main title for the spectrogram. Default is not title.
#' @param wl numeric, window length in samples. Default is `512`.
#' @param ovl numeric, overlap in samples. Default is `450`.
#' @param ylim numeric vector of length 2, limits for the y-axis. Default is no limits ('free').
#' @param xlim numeric vector of length 2, limits for the x-axis. Default is no limits ('free').
#' @param mar numeric vector of length 4, the margins of the plot for the `impagep` function.
#' Default is `rep(3, 4)`.
#'
#' @return Plots the spectrogram to current window.
#'
#' @export
#'
#' @importFrom signal "specgram"
#' @importFrom oce "imagep"
#' @importFrom grDevices "hcl.colors"

better.spectro = function(wave,
                          main = '',
                          wl = 512,
                          ovl = 450,
                          xlim = 'free',
                          ylim = 'free',
                          mar = rep(3, 4)){

  # Create spectrogram
  n = wl
  spec = signal::specgram(x = wave@left,
                  n = n,
                  Fs = wave@samp.rate,
                  window = wl,
                  overlap = ovl)

  # Discard phase information
  P = abs(spec$S)

  # Normalize
  P = P/max(P)

  # Convert to dB
  P = 20*log10(P)

  # Fix broken time axis
  spec$t = spec$t + wl/wave@samp.rate/2 # - ovl/wave@samp.rate/2

  # Find limits
  if(xlim[1] == 'free') xlim = c(min(spec$t), max(spec$t))
  if(ylim[1] == 'free') ylim = c(min(spec$f), max(spec$f))

  # Plot spectrogram
  oce::imagep(x = spec$t,
         y = spec$f,
         z = t(P),
         ylab = 'Frequency [Hz]',
         xlab = 'Time [s]',
         zlim = c(min(P), max(P)),
         xlim = xlim,
         ylim = ylim,
         col = hcl.colors(20, "RdBu", rev = TRUE) ,
         drawPalette = F,
         decimate = F,
         main = main,
         mar = mar,
         cex.main = 0.5)

}
