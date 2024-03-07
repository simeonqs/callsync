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
#' wave = load.wave(local_file_1)
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
