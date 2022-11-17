# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper 
# Date started: 15-11-2021
# Date last modified: 15-11-2022
# Author: Simeon Q. Smeele
# Description: Detects calls in a wave object with amplitude envelope. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

call.detect = function(wave, # wave object
                       threshold = 0.3, # fraction of max of envelope to use as threshold for start/end
                       # if vector of two is supplied, the first is used for start
                       # and second for end (in case of echo)
                       msmooth = c(500, 95), # smoothening of envelope
                       plot_it = FALSE # if TRUE, returns three-panel plot of wave, envelope and spectrogram
){
  
  # Create envelope
  env = env(wave, msmooth = msmooth, plot = F) 
  env = ( env - min(env) ) / max( env - min(env) )
  duration = length(wave@left)/wave@samp.rate
  
  # Find max location
  where_max = which(env == 1)
  ## Left loop
  start = NA
  j = where_max
  while(is.na(start)){
    j = j - 1
    if(j == 0) start = j else if(env[j] < threshold[1]) start = j
  } 
  ## Right loop
  end = NA
  j = where_max
  while(is.na(end)){
    j = j + 1
    if(j == length(env)) end = j else if(env[j] < threshold[length(threshold)]) end = j
  } 
  
  # Re-clip wave
  start_env = start
  if(start == 0) start = 1 # avoid issues when call starts at start clip
  end_env = end
  start = round((start-1) * duration/length(env) * wave@samp.rate)
  end = round((end-1) * duration/length(env) * wave@samp.rate)
  new_wave = wave[start:end]
  
  # Plot
  if(plot_it){
    par(mfrow = c(2, 2))
    plot(env, type = 'l')
    abline(v = c(start_env, end_env))
    abline(h = threshold, lty = 2)
    better.spectro(wave, wl = 512, ovl = 450, ylim = c(500, 4000))
    abline(v = c(start/wave@samp.rate, end/wave@samp.rate))
    plot(wave)
    abline(v = c(start/wave@samp.rate, end/wave@samp.rate))
  }
  
  # Return
  detections = data.frame(start = start, end = end)
  return(detections)
  
}