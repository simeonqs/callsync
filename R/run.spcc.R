#' @title run.spcc
#'
#' @description Runs spectrograph cross correlation on multiple wave objects.
#'
#' @param waves a list of wave objects, e.g., from `lapply` in combination with
#' `load.wave` or `readWave`.
#' @param freq_range numeric vector of length 2, the frequency range in Hz to
#' return.
#' @param wl numeric, window length in samples. Default is `512`.
#' @param ovl numeric, overlap in samples. Default is `450`.
#' @param method character, either `sd` or `max`. If `sd`, pixels are
#' standardised. If `max`, pixels are
#' normalised.
#' @param thr_low numeric, the lower range (see `method`). Pixels with lower
#' values are set to 0 for noise
#' reduction.
#' @param thr_high numeric, the upper range (see `method`). Pixels with higher
#' values are set to `thr_high`.
#' @param sum_one logical, if `TRUE` pixels are divided by the sum of all
#' pixels, such that they sum to one.
#' @param mc.cores numeric, how many threads to run in parallel. For Windows
#' only one can be used.
#' @param step_size numeric, argument for `sliding.pixel.comparison` how many
#' pixels should be moved for each
#' step. Default is `10`.
#'
#' @return Matrix with row and columns names equal to the names of the wave
#' list. Diagonal is zeroes. Other values are the normalised pairwise
#' distances from `sliding.pixel.comparison`.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' file = system.file("extdata", "", package = "callsync")
#' files = list.files(file, "wave_*", full.names = T)
#' waves = lapply(files, load.wave)
#' spcc_out = run.spcc(waves)
#'
#' @export
#'
#' @importFrom signal "specgram"
#' @importFrom oce "imagep"
#' @importFrom parallel "mclapply"
#' @importFrom utils "combn"

run.spcc = function(waves,
                    freq_range = c(700, 3500),
                    thr_low = 0.45,
                    thr_high = 0.6,
                    wl = 256,
                    ovl = 250,
                    method = 'sd',
                    sum_one = TRUE,
                    mc.cores = 1,
                    step_size = 10
){

  # Generate spec_ojects
  spec_objects = lapply(waves, create.spec.object,
                        freq_range = freq_range, plot_it = FALSE,
                        thr_low = thr_low, thr_high = thr_high,
                        wl = wl, ovl = ovl, method = method, sum_one = sum_one)

  # Get combinations and run function
  c = combn(1:length(spec_objects), 2)
  o = mclapply(1:ncol(c), function(i)
    sliding.pixel.comparison(spec_objects[[c[1,i]]], spec_objects[[c[2,i]]],
                             step_size = step_size),
    mc.cores = mc.cores) |> unlist()
  o = o/max(o)

  # Create matrix
  if(is.null(names(waves))) names(waves) = seq_along(waves)
  m = o.to.m(o, str_remove(names(waves), '.wav'))

  # Return
  return(m)

} # end run.spcc
