#' @title create.spec.object
#'
#' @description Creates a tailored spectrogram (matrix) for spectrographic cross correlation.
#'
#' @param wave wave object, e.g., from `load.wave` or `readWave`.
#' @param freq_range numeric vector of length 2, the frequency range in Hz to return.
#' @param hop integer, how many samples to skip for each trace point.
#' @param wl numeric, window length in samples. Default is `512`.
#' @param ovl numeric, overlap in samples. Default is `450`.
#' @param plot_it  logical, if `TRUE`, returns three-panel plot of wave form, envelope and spectrogram to
#' current plotting window. Default is `FALSE`.
#' @param method character, either `sd` or `max`. If `sd`, pixels are standardised. If `max`, pixels are
#' normalised.
#' @param thr_low numeric, the lower range (see `method`). Pixels with lower values are set to 0 for noise
#' reduction.
#' @param thr_high numeric, the upper range (see `method`). Pixels with higher values are set to `thr_high`.
#' @param sum_one logical, if `TRUE` pixels are divided by the sum of all pixels, such that they sum to one.
#'
#' @return Returns a numeric matrix with the spectrogram values.
#'
#' @export
#'
#' @importFrom signal "specgram"
#' @importFrom oce "imagep"

create.spec.object = function(wave,
                              wl = 512,
                              ovl = 450,
                              freq_range = c(0, 20000),
                              plot_it = T,
                              thr_low = 1.5,
                              thr_high = 3,
                              sum_one = F,
                              method = 'sd'){

  # Create spectrogram
  n = wl
  spec = specgram(x = wave@left,
                  n = n,
                  Fs = wave@samp.rate,
                  window = wl,
                  overlap = ovl)

  # Discard phase information
  P = abs(spec$S)

  # Convert to dB
  P = 20*log10(P)

  # Cut spectrogram
  spec_object = as.matrix(P)
  spec_object = spec_object[which(spec$f > freq_range[1] & spec$f < freq_range[2]),]
  if(method == 'max') spec_object = (spec_object - mean(spec_object)) / max(spec_object)
  if(method == 'sd') spec_object = (spec_object - mean(spec_object)) / sd(spec_object)
  spec_object[spec_object < thr_low] = 0 # thr_low
  spec_object[spec_object > thr_high] = thr_high
  if(sum_one) spec_object = spec_object/sum(spec_object)

  # Plot spectrogram
  if(plot_it) image(t(spec_object), col = hcl.colors(12, 'Blue-Yellow', rev = T))

  # Return
  return(spec_object)

}
