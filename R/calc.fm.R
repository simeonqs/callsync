#' @title calc.fm
#'
#' @description Calculates the frequency modulation for a wave object and returns several measurements in
#' a data frame.
#'
#' @param trace numeric vector, e.g., the fundamental frequency from `trace.fund`.
#' @param min_height the minimum difference between a bottom and a peak for an infliction point to be
#' accepted.
#' @param plot_it logical, if `TRUE` plot the trace and peaks to current window. Default is `FALSE`.
#'
#' @return Returns a data frame with fm = median difference between peaks and bottoms and ipi = inter peak,
#' np = number of peaks.
#' interval (s).
#'
#' @importFrom stats "median"
#' @importFrom graphics "points"
#'
#' @export

calc.fm = function(trace,
                   min_height = 8,
                   plot_it = FALSE){

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
    np = NA
  } else {
    infl = rbind(peaks, bottoms)
    infl = infl[order(infl[,1]),]
    fm = median(diff(infl))
    ipi = median(diff(peaks[,1]))
    np = length(peaks)
  }

  # Return
  return(list(fm = fm,
              ipi = ipi,
              np = np))

} # end calc.fm
