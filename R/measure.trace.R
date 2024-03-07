#' @title measure.trace
#'
#' @description Takes several measurements on a fundamental frequency trace.
#'
#' @param trace data frame, e.g., the output of the `trace.fund` function.
#' Should contain columns with time = time in seconds, fund = fundamental
#' frequency in Hz and missing = logical indicating if the fundamental was
#' detected (`TRUE`) or interpolated (`FALSE`).
#' @param sr sample rate of the wave object used for `trace.fund`.
#' @param hop the `hop` parameter used to generate the trace.
#'
#' @return Returns a dataframe with all measurements.
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
#' result = measure.trace(trace)
#'
#' @importFrom graphics "abline"
#'
#' @export

measure.trace = function(trace, sr = 44100, hop = 5){

  # Calculate FM
  temp = calc.fm(trace = trace$fund, min_height = 5, plot_it = FALSE)

  # Create data frame
  out = data.frame(mean_fund_hz = mean(trace$fund),
                   duration_s = max(trace$time),
                   band_hz = max(trace$fund) - min(trace$fund),
                   max_freq_hz = max(trace$fund),
                   min_freq_hz = min(trace$fund),
                   diff_start_mean = trace$fund[1] - mean(trace$fund),
                   diff_end_mean = trace$fund[nrow(trace)] -
                     mean(trace$fund),
                   ipi_s = as.numeric(temp$ipi * hop / sr),
                   fm_hz = as.numeric(temp$fm),
                   sd_trace = sd(trace$fund),
                   prop_missing_trace = length(which(trace$missing))/
                     length(trace$missing))

  # Return
  return(out)

}
