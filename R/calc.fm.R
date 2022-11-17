# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: cockatiels Stephen
# Date started: 14-06-2022
# Date last modified: 14-06-2022
# Author: Simeon Q. Smeele
# Description: This function detects peaks and bottoms, and calculates inter peak interval and fm hight. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


calc.fm = function(trace, min_height = 8, plot_it = F){
  
  # Plot
  if(plot_it) plot(trace)
  
  # Open storage
  peaks = matrix(nrow = 0, ncol = 2)
  bottoms = matrix(nrow = 0, ncol = 2)
  
  # Run up and down
  temp_max = temp_min = trace[1]
  temp_peak = temp_bottom = c(NA, NA)
  for(i in 2:length(trace)){
    
    # Search for peak
    if(all(is.na(temp_peak))) if(trace[i] - temp_min > min_height){
      temp_peak = c(i, trace[i]) # save peak as temp
      if(!all(is.na(temp_bottom))){
        bottoms = rbind(bottoms, temp_bottom) # save last temp bottom
        temp_bottom = c(NA, NA) # reset
      }
    } 

    # Update peak
    if(!all(is.na(temp_peak))) if(trace[i] > temp_peak[2]) temp_peak = c(i, trace[i])
    
    # Update temp max
    if(!all(is.na(temp_peak))) temp_max = temp_peak[2] else 
      if(nrow(peaks) > 0) temp_max = peaks[nrow(peaks), 2]  
    
    # Search for bottom
    if(all(is.na(temp_bottom))) if(temp_max - trace[i] > min_height){
      temp_bottom = c(i, trace[i])
      if(!all(is.na(temp_peak))){
        peaks = rbind(peaks, temp_peak) # save last temp peak
        temp_peak = c(NA, NA) # reset
      }
    } 
    
    # Update bottom
    if(!all(is.na(temp_bottom))) if(trace[i] < temp_bottom[2]) temp_bottom = c(i, trace[i])
    
    # Update temp min
    if(!all(is.na(temp_bottom))) temp_min = temp_bottom[2] else 
      if(nrow(bottoms) > 0) temp_min = bottoms[nrow(bottoms), 2]  
    
  } # end i loop
  
  # Plot
  if(plot_it){
    points(peaks, col = 2, pch = 16)
    points(bottoms, col = 3, pch = 16)
  }
  
  # Calculate FM and IPI
  if(nrow(peaks) < 2){
    ipi = NA
    fm = NA
  } else {
    infl = rbind(peaks, bottoms)
    infl = infl[order(infl[,1]),]
    fm = median(diff(infl))
    ipi = median(diff(peaks[,1]))
  }
  
  # Return
  return(list(fm = fm, ipi = ipi))
  
} # end calc.fm