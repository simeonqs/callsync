#' @title load.wave
#'
#' @description Wrapper function for `readWave` from *tuneR*.
#' Also optionally applies `ffilter` from *seewave*.
#'
#' @param path_audio_file the path to the .wav file
#' @param from time in seconds from where to start the loading of the audio file.
#' Default is `0` which loads the whole file.
#' @param to time in seconds until where to load the audio file. Default is `Inf` which loads the whole file.
#' @param ffilter_from numeric, frequency in Hz for the high-pass filter.
#' Default is `NULL`, which does not apply a filter.
#'
#' @return Returns an R wave object.
#'
#' @export
#' @importFrom tuneR "readWave"
#' @importFrom seewave "ffilter"

load.wave = function(path_audio_file,
                     from = 0,
                     to = Inf,
                     ffilter_from = NULL){

  wave = readWave(path_audio_file,
                  from = as.numeric(from),
                  to = as.numeric(to),
                  units = 'seconds')

  if(!is.null(ffilter_from)) wave = ffilter(wave, from = ffilter_from, output = 'Wave')

  return(wave)

}
