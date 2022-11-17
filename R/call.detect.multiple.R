# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods papers 
# Date started: 15-11-2021
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: Finds multiple calls in a wav object. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

call.detect.multiple = function(wave, # wave object
                                threshold = 0.3, # fraction of max of envelope to use as threshold for start/end
                                # if vector of two is supplied, the first is used for start
                                # and second for end (in case of echo)
                                msmooth = c(500, 95), # smoothening of envelope
                                plot_it = F, # if T, returns three-panel plot of wave, envelope and spectrogram
                                min_dur = 0.01, # minimal duration of an item 
                                return_times = F){ # if T, outputs start and end to global environment
  
  # Envelope
  env = env(wave, msmooth = msmooth, plot = F) 
  env = ( env - min(env) ) / max( env - min(env) )
  duration = length(wave@left)/wave@samp.rate
  
  # Find calls
  which_above = which(env > threshold)
  start = which_above[1]
  end = which_above[length(which_above)]
  other_starts = which_above[which(diff(which_above) != 1)+1]
  other_ends = which_above[which(diff(which_above) != 1)]
  starts = sort(c(start, other_starts))
  ends = sort(c(end, other_ends))
  # Remove short durations
  r = c()
  if(length(starts) > 1){
    for(j in 1:length(starts)){
      dif = ends[j] - starts[j]
      if(dif * duration/length(env) < min_dur) r = c(r, j)
    }
    if(length(r) > 0){
      starts = starts[-r]
      ends = ends[-r]
    }
  } 
  
  # Fix time
  if(length(starts) != 0){
    starts = round((starts-1) * duration/length(env) * wave@samp.rate)
    ends = round((ends-1) * duration/length(env) * wave@samp.rate)
  }
  
  # Return
  detections = data.frame(start = starts, end = starts)
  return(detections)
  
}
