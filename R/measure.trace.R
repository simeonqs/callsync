# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper 
# Date started: 16-11-2021
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: This function takes several measurements on a fundamental frequency trace and returns
# the results as a dataframe. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

measure.trace = function(trace, sr = 44100){ 
  
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