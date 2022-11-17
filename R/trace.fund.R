# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper 
# Date started: 16-03-2021
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: Traces the fundamental frequency from a wave object. Also applies smoothening to trace.
# Returns new_trace, which is a dataframe with time in seconds and fund in Herz. 
# This version has a control to remove noisy peaks.
# This version includes a noise factor that can be changed. 
# This version is fixed if no points could be detected. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(seewave)

trace.fund = function(new_wave, # wave object
                      hop = 5, # step size of sliding window
                      wl = 200, # window length for spectrum
                      freq_lim = c(1.1, 4), # frequency limits in kHz to find fundamental
                      spar = 0.4, # how much to smooth, higher = more smooth
                      noise_factor = 3.5, # how much more than the average the peak should be
                      thr = 0.3){ # threshold for detection
  
  # Trace
  final = floor(length(new_wave@left) - wl - 1)
  starts = seq(1, final, hop)
  funds = sapply(starts, function(start){
    w = new_wave[start:(start+wl)] # select part wave
    s = seewave::spec(w, plot = F) # make spectrum
    s[,2][s[,1] < freq_lim[1] | s[,1] > freq_lim[2]] = 0 # set all outside limit to 0
    s[,2] = s[,2]/max(s[,2]) # rescale the remaining spectrum
    peak = s[,1][s[,2] > thr][1]
    cont = T
    ii = which(s[,1] == peak)
    while(cont){ # climb until reach first peak
      if(s[ii+1,2] > s[ii,2]) ii = ii + 1 else {
        peak = s[ii,1]
        cont = F
      }
    }
    ## Only include peak if very clear
    peak = ifelse(s[ii,2] > noise_factor * mean(s[,2][s[,2]!=0]), peak, NA)
    return(peak)
  })
  
  if(length(which(!is.na(funds))) < 4) funds = rep(1, length(funds)) # fix if all points are missing
  
  d = data.frame(starts, funds)
  smoo = with(d[!is.na(d$funds),], smooth.spline(starts, funds, spar = spar))
  new_trace = with(d, predict(smoo, starts))$y
  
  # Return
  out = data.frame(time = (starts + wl/2) / new_wave@samp.rate,
                   fund = new_trace * 1000,
                   missing = ifelse(is.na(funds), T, F))
  if(all(funds == 1)) out$missing = rep(T, length(out$missing))
  return(out)
  
}