#' @title sliding.pixel.comparison
#'
#' @description Can be used to run spectrographic cross correlation. Both
#' spectrograms are zero-padded and slid over each other. For each step the
#' difference is computed. The function returns the absolute difference at the
#' point at the minimum (maximal signal overlap).
#'
#' @param s1 numeric matrix, the first spectrogram.
#' @param s2 numeric matrix, the second spectrogram.
#' @param step_size numeric, how many pixels should be moved for each step.
#' Default is `1`.
#'
#' @return Returns the distance at the point of maximal signal overlap.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' path_git = 'https://raw.githubusercontent.com'
#' path_repo = '/simeonqs/callsync/master/tests/testthat/files'
#' file_1 = '/wave_1.wav'
#' file_2 = '/wave_2.wav'
#' url_1 = paste0(path_git, path_repo, file_1)
#' url_2 = paste0(path_git, path_repo, file_2)
#' local_file_1 = paste(tempdir(), file_1, sep = '/')
#' local_file_2 = paste(tempdir(), file_2, sep = '/')
#' if(!file.exists(local_file_1))
#'   download.file(url_1, destfile = local_file_1, mode = 'wb',)
#' if(!file.exists(local_file_2))
#'   download.file(url_2, destfile = local_file_2, mode = 'wb')
#' wave_1 = readWave(local_file_1)
#' wave_2 = readWave(local_file_2)
#' so_1 = create.spec.object(wave = wave_1, plot_it = FALSE)
#' so_2 = create.spec.object(wave = wave_2, plot_it = FALSE)
#' out = sliding.pixel.comparison(so_1, so_2)
#'
#' @export

sliding.pixel.comparison = function(s1,
                                    s2,
                                    step_size = 1){

  # Run through positions
  np = ncol(s1) + ncol(s2) - 1
  os = sapply(seq(1, np, step_size), function(i){
    m = min(s1, s2)
    r = nrow(s1)
    t1 = cbind(matrix(m, ncol = max(0, i - ncol(s1)), nrow = r),
               s1,
               matrix(m, ncol = max(0, ncol(s2) - i), nrow = r))
    t2 = cbind(matrix(m, ncol = max(0, ncol(s1) - i), nrow = r),
               s2,
               matrix(m, ncol = max(0, i - ncol(s2)), nrow = r))
    o = sum(abs(t2 - t1))
    return(o)
  })

  return(min(os))

} # end sliding.pixel.comparison
