#' @title better.spectro
#'
#' @description Creates a spectrogram and plots it to the current window.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param main character, title for the spectrogram. Default is no title.
#' @param wl numeric, window length in samples. Default is `512`.
#' @param ovl numeric, overlap in samples. Default is `wl/2`.
#' @param ylim numeric vector of length 2, limits for the y-axis. Default is
#' no limits.
#' @param xlim numeric vector of length 2, limits for the x-axis. Default is
#' no limits.
#' @param mar numeric vector of length 4, the margins of the plot for the
#' `impagep` function. Default is `rep(3, 4)`.
#' @param cex.main numeric, the relative size of the title
#' @param cex.lab numeric, the relative size of the axis titles
#' @param cex.axis numeric, the relative size of the axis labels.
#' @param col function, col argument for the `imagep` function.
#' Default is `hcl.colors(20, palette = "RdBu", rev = TRUE)`.
#'
#' @return Plots the spectrogram to current window.
#'
#' @examples
#'
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
#' better.spectro(wave)
#'
#' @export
#'
#' @importFrom signal "specgram"
#' @importFrom oce "imagep"
#' @importFrom grDevices "hcl.colors"

better.spectro = function(wave,
                          main = "",
                          wl = 512,
                          ovl = wl/2,
                          xlim = NULL,
                          ylim = NULL,
                          mar = rep(3, 4),
                          cex.main = 1,
                          cex.axis = 0.75,
                          cex.lab = 0.5,
                          col = hcl.colors(20, palette = "RdBu", rev = TRUE)){

  # Warning if ovl >= wl
  if(ovl >= wl) stop('\n\nOverlap greater than window length.
                     \nPlease make sure that ovl < wl.')

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
  if(is.null(xlim[1])) xlim = c(min(spec$t), max(spec$t))
  if(is.null(ylim[1])) ylim = c(min(spec$f), max(spec$f))

  # Plot spectrogram
  if(min(P) == -Inf) warning('min(P) is -Inf, something might be wrong.')
  oce::imagep(x = spec$t,
              y = spec$f,
              z = t(P),
              ylab = 'Frequency [Hz]',
              xlab = 'Time [s]',
              zlim = c(max(min(P), -200), max(P)),
              xlim = xlim,
              ylim = ylim,
              col = col,
              drawPalette = FALSE,
              decimate = FALSE,
              main = main,
              mar = mar,
              cex.main = cex.main,
              cex.axis = cex.axis,
              cex.lab = cex.lab)

}
