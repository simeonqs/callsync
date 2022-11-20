#' @title measure.trace
#'
#' @description Takes several measurements on a fundamental frequency trace.
#'
#' @param trace data frame, e.g., the output of the `trace.fund` function. Should contain columns with
#' time = time in seconds, fund = fundamental frequency in Hz and missing = logical indicating if the
#' fundamental was detected (`T`) or interpolated (`F`).
#' @param sr sample rate of the wave object used for `trace.fund`.
#'
#' @return Returns a data frame with all measurements.
#'
#' @export

measure.trace.multiple = function(traces,
                                  new_waves,
                                  waves,
                                  snr = 10,
                                  path_pdf = NULL){

  # Make data frame to save results
  measurements = data.frame()

  # Run through files
  if(!is.null(path_pdf)) pdf(path_pdf, 7, 5)
  for(i in 1:length(audio_files)){

    # Load wave
    new_wave = new_waves[[i]]
    start = detections[[i]]$start
    end = detections[[i]]$end

    # Test STN
    signal = mean(abs(new_wave@left))
    noise = mean(abs(waves[[i]][-(start:end)]@left))
    if(signal/noise > snr) col = 1 else col = 2

    # Plot
    if(!is.null(path_pdf)){
      par(mfrow = c(2, 2))
      plot(waves[[i]])
      abline(v = c(start/waves[[i]]@samp.rate, end/waves[[i]]@samp.rate), col = col)
      better.spectro(waves[[i]], wl = 200, ovl = 195, ylim = c(500, 4000),
                     main = basename(audio_files[i]), mar = rep(4, 4))
      lines(traces[[i]]$time + start/waves[[i]]@samp.rate,
            traces[[i]]$fund,
            col = alpha('green', 0.3), lty = 1, lwd = 1)
      abline(h = c(mean(traces[[i]]$fund, na.rm = T),
                   max(traces[[i]]$fund, na.rm = T),
                   min(traces[[i]]$fund, na.rm = T)), lty = 2,
             col = alpha('black', 0.5))
      plot(NULL, xlim = c(0, 1), ylim = c(0, 8), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
      text(0, 1:6, adj = 0,
           labels = c(sprintf('mean_fund_hz: %s', round(mean(traces[[i]]$fund, na.rm = T))),
                      sprintf('diff_start_mean: %s',
                              round(traces[[i]]$fund[1] - mean(traces[[i]]$fund))),
                      sprintf('diff_end_mean: %s',
                              round(traces[[i]]$fund[nrow(traces[[i]])] -
                                      mean(traces[[i]]$fund))),
                      sprintf('duration: %s', round((end - start)/waves[[i]]@samp.rate, 2)),
                      sprintf('band_hz: %s',
                              round(max(traces[[i]]$fund) - min(traces[[i]]$fund))),
                      sprintf('prop_missing_trace: %s', round(length(which(traces[[i]]$missing))/
                                                                length(traces[[i]]$missing), 3))))
    } # end plot_it

    # Take measurements and save results
    temp = measure.trace(traces[[i]], sr = waves[[i]]@samp.rate) # take measurements
    temp = cbind(temp, data.frame(file = basename(audio_files[i]), # add other info
                                  signal_to_noise = signal/noise))
    measurements = rbind(measurements, temp) # save in main dataframe

  } # end i loop
  if(!is.null(path_pdf)) dev.off()

  # Return
  return(measurements)

} # end measure.trace.multiple
