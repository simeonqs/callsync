#' @title detect.and.assign
#'
#' @description Traces the fundamental frequency from a wave object. Also
#' applies smoothening to trace.
#'
#' @param all_files character vector or `NULL`. Character vector should contain
#' all the paths to the raw recordings that should be considered. If `NULL`
#' files are loaded from `path_chunks`.
#' @param path_chunks character, path to where the chunks are stored.
#' @param path_calls character, path to where to store the results.
#' @param ffilter_from numeric, frequency in Hz for the high-pass filter.
#' @param threshold numeric, threshold (fraction of the maximum) for amplitude
#' envelope when detecting call.
#' @param msmooth used as argument for the `seewave::env` function. *A vector
#' of length 2 to smooth the amplitude envelope with a mean sliding window. The
#' first component is the window length (in number of points). The second
#' component is the overlap between successive windows (in \%).* Default is
#' `c(500, 95)`.
#' @param min_dur numeric, the minimal duration in seconds for a detection to
#' be saved. Default is `0.1`.
#' @param max_dur numeric, the maximal duration in seconds for a detection to
#' be saved. Default is `0.3`.
#' @param step_size numeric, duration in seconds of the bins for signal
#' compression before cross correlation. Default is `0.01`.
#' @param wing numeric, the duration in seconds to load before and after each
#' detection to improve alignment. This is not saved with the aligned call.
#' @param save_files logical, if `TRUE` the files are stored in the
#' `path_chunks` location. Results are also returned.
#' @param quiet logical, if `TRUE` no messages are printed.
#' @param save_extra numeric, how much to add to start and end time in
#' seconds. Can be used to make sure the whole vocalisation is included.
#'
#' @return Returns a data frame with start = start time in samples and end =
#' end time in samples for each detection.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' path_git = 'https://raw.githubusercontent.com'
#' path_repo = '/simeonqs/callsync/master/tests/testthat/files'
#' file_1 = '/chunk@1@1@1@1.wav'
#' file_2 = '/chunk@2@1@1@1.wav'
#' url_1 = paste0(path_git, path_repo, file_1)
#' url_2 = paste0(path_git, path_repo, file_2)
#' local_file_1 = paste(tempdir(), file_1, sep = '/')
#' local_file_2 = paste(tempdir(), file_2, sep = '/')
#' if(!file.exists(local_file_1))
#'   download.file(url_1, destfile = local_file_1, mode = 'wb',)
#' if(!file.exists(local_file_2))
#'   download.file(url_2, destfile = local_file_2, mode = 'wb')
#' all_files = c(local_file_1, local_file_2)
#' \dontrun{
#' result = detect.and.assign(all_files = all_files,
#'                            quiet = TRUE,
#'                            save_files = FALSE)
#'                            }
#'
#' @export
#'
#' @importFrom seewave "env"
#' @importFrom tuneR "readWave"
#' @importFrom tuneR "writeWave"
#' @importFrom stringr "str_detect"

detect.and.assign = function(all_files = NULL,
                             path_chunks = NULL,
                             path_calls = NULL,
                             ffilter_from = 1100,
                             threshold = 0.4,
                             msmooth = c(1000, 95),
                             min_dur = 0.1,
                             max_dur = 0.30,
                             step_size = 0.01,
                             wing = 6,
                             save_files = TRUE,
                             quiet = FALSE,
                             save_extra = 0

){

  # Make sure a path is supplied if the files should be saved
  if(save_files & is.null(path_calls))
    stop('If you want to save the files you need set path_calls.')

  # List files and detect recording IDs
  if(is.null(all_files))
    all_files = list.files(path_chunks, pattern = '*wav',
                           full.names = TRUE, recursive = TRUE)
  all_recs = all_files |> strsplit('@') |> sapply(`[`, 3)

  # Detect calls in each chunk
  if(!quiet)
    message(sprintf('Running detections on %s chunks.', length(all_recs)))
  detections = lapply(all_files, function(file){
    if(!quiet) message(sprintf('Running: %s', file))
    wave = load.wave(file, ffilter_from = ffilter_from)
    detections = call.detect.multiple(wave = wave,
                                      threshold = threshold,
                                      msmooth = msmooth,
                                      min_dur = min_dur,
                                      max_dur = max_dur,
                                      plot_it = FALSE,
                                      save_extra = save_extra)
    return(detections)
  })
  names(detections) = basename(all_files)

  # Assign calls
  detec_saver = call.assign(all_files = all_files,
                            detections = detections,
                            save_files = save_files,
                            path_calls = path_calls,
                            wing = wing,
                            step_size = step_size,
                            quiet = quiet)

  # Return
  if(!save_files) return(detec_saver)

  # Message
  if(!quiet) message('All done!')

} # end detect.and.assign
