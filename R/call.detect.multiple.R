#' @title call.detect.multiple
#'
#' @description Detects multiple calls in a wave object using an amplitude
#' envelope.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param threshold rector of length 1 or 2. The fraction of the maximum of the
#' normalised envelope to use as threshold to detect start and end. If a vector
#' of length 2 is supplied, the first is used to detect the start and the
#' second to detect the end (in case of echo).
#' @param msmooth used as argument for the `seewave::env` function. *A vector
#' of length 2 to smooth the amplitude envelope with a mean sliding window. The
#' first component is the window length (in number of points). The second
#' component is the overlap between successive windows (in \%).* Default is
#' `c(500, 95)`.
#' @param plot_it  logical, if `TRUE`, returns three-panel plot of wave form,
#' envelope and spectrogram to current plotting window. Default is `FALSE`.
#' @param min_dur numeric, the minimal duration in seconds for a detection to
#' be saved. Default is `0.1`.
#' @param max_dur numeric, the maximal duration in seconds for a detection to
#' be saved. Default is `0.3`.
#' @param save_extra numeric, how much to add to start and end time in seconds.
#' Can be used to make sure the whole vocalisation is included.
#' @param env_type character, what type of envelope to calculate. If `Hilbert`
#' returns the modulus (Mod)
#' of the analytical signal of wave obtained through the Hilbert transform
#' (hilbert) using seewave::env. If `summed` returns the summed absolute
#' amplitude. Default is `Hilbert`.
#' @param bin_depth numeric, how many samples to sum if env_type is `summed`.
#' Default is `512`.
#' @param merge_overlap logical, if `TRUE` overlapping detections (due to
#' `save_extra`) are merged. Default is `FALSE`.
#'
#' @return Returns a data frame with start = start time in samples and end =
#' end time in samples for each detection. Optionally also plots the wave form
#' and detections to current window.
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
#' cd = call.detect.multiple(wave)
#'
#' @export
#'
#' @importFrom seewave "env"
#' @importFrom scales "alpha"

call.detect.multiple = function(wave,
                                threshold = 0.3,
                                msmooth = c(500, 95),
                                plot_it = FALSE,
                                min_dur = 0.1,
                                max_dur = 0.3,
                                save_extra = 0,
                                env_type = 'Hilbert',
                                bin_depth = 512,
                                merge_overlap = FALSE){

  # Envelope
  if(!env_type %in% c('Hilbert', 'summed')){
    warning('Unknown env type. Defaulting to Hilbert.')
  }
  if(env_type == 'Hilbert'){
    env = env(wave, msmooth = msmooth, plot = FALSE)
    env = (env - min(env)) / max(env - min(env))
  }
  if(env_type == 'summed') {
    starts = seq(1, length(wave@left), by = bin_depth)
    env = vapply(seq_along(starts), function(i){
      start = starts[i]
      end = ifelse(i == length(starts), length(wave), starts[i+1])
      sum(abs(wave@left[start:end]))
    }, numeric(1))
    env = env / bin_depth
  }
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
    starts = round((starts-1) * duration/length(env) *
                     wave@samp.rate - save_extra * wave@samp.rate)
    starts[starts<1] = 1
    ends = round((ends-1) * duration/length(env) *
                   wave@samp.rate + save_extra * wave@samp.rate)
    ends[ends>length(wave@left)] = length(wave@left)
  }

  # Optionally merge detections if they overlap because of save_extra
  if(merge_overlap & length(starts) > 1){
    i = 1
    while(i < length(starts)){
      for(i in seq(2, length(starts))){
        ## if overlap make end previous to end current
        if(starts[i] < ends[i-1]){
          ends[i-1] = ends[i]
          ## remove this detection
          starts = starts[-i]
          ends = ends[-i]
          break
        } # end if start < end
      } # end for i
    } # end while i
  } # end if merge_overlap

  # Optionally plot output
  if(plot_it){
    oldpar = par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(2 , 1), mar = c(0, 0, 0, 0), oma = c(4, 4, 1, 1))
    plot(env, type = 'l', xaxt = 'n')
    abline(h = threshold, lty = 2, col = 2)
    plot(wave)
    for(i in seq_len(length(starts))){
      graphics::rect(xleft = starts[i]/wave@samp.rate,
                     xright = ends[i]/wave@samp.rate,
                     ybottom = par('usr')[3], ytop = par('usr')[4],
                     border = NA, col = alpha('#3a586e', 0.5))
      abline(v = starts[i]/wave@samp.rate, lty = 2,
             col = '#3a586e', lwd = 3)
      abline(v = ends[i]/wave@samp.rate, lty = 2,
             col = '#3a586e', lwd = 3)
    }
    mtext('time [s]', 1, 3)
  }

  # Return, if no detections, create empty data.frame
  if(length(starts > 0))
     detections = data.frame(start = starts, end = ends) else
       detections = data.frame(start = numeric(), end = numeric())
  return(detections)

}
