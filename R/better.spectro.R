# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: monk parakeets
# Date started: 10-12-2020
# Date last modified: 21-05-2022
# Author: Simeon Q. Smeele
# Description: Creates spectrogram from wave object. Based on code from chapter I.
# This version has the requires
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(oce)
require(signal)

better.spectro = function(wave, 
                          main = ' ', 
                          wl = 512, 
                          ovl = 450, 
                          xlim = 'free', 
                          ylim = 'free',
                          mar = rep(3, 4)){
  
  # Create spectrogram
  n = wl
  spec = specgram(x = wave@left,
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
  if(xlim[1] == 'free') xlim = c(min(spec$t), max(spec$t))
  if(ylim[1] == 'free') ylim = c(min(spec$f), max(spec$f))
  
  # Plot spectrogram
  imagep(x = spec$t,
         y = spec$f,
         z = t(P),
         ylab = 'Frequency [Hz]',
         xlab = 'Time [s]',
         zlim = c(min(P), max(P)),
         xlim = xlim,
         ylim = ylim,
         col = hcl.colors(20, "RdBu", rev = TRUE) ,
         drawPalette = F,
         decimate = F,
         main = main,
         mar = mar,
         cex.main = 0.5) 
  
}
