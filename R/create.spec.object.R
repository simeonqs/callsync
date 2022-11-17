# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper
# Date started: 10-12-2020
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: Creates spectrogram from wave object.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

create.spec.object = function(wave, # a wave object
                              wl = 512, # window length in n-samples
                              ovl = 450, # overlap between windows in n-samples
                              freq_range = c(0, 20000), # what range to return
                              plot_it = T, # whether or not to plot 
                              thr_low = 1.5, # lower threshold for pixels
                              thr_high = 3, # upper threshold for pixels
                              sum_one = F, # if T, sets sum of all pixels to one
                              method = 'sd'){ # how to normalise/standardise 
  
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
