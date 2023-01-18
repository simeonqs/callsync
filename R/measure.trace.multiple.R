#' @title measure.trace.multiple
#'
#' @description Takes several measurements on multiple fundamental frequency traces.
#'
#' @param traces a list of data frames, e.g., the output of the `trace.fund` function. Should contain columns
#' with time = time in seconds, fund = fundamental frequency in Hz and missing = logical indicating if the
#' fundamental was detected (`TRUE`) or interpolated (`FALSE`). If the list is named the names will be used
#' as file names in the output.
#' @param new_waves a list of wave objects, should only contain the call.
#' @param waves a list of wave objects, should not be resized.
#' @param detections the detections.
#' @param sr numeric, sample rate of the waves objects used for the traces. Only needed if `waves` is `NULL`.
#' @param path_pdf numeric or `NULL`, where to store the pdf. If `NULL` no pdf is stored.
#'
#' @return Returns a data frame with all measurements.
#'
#' @importFrom graphics "abline"
#' @importFrom graphics "lines"
#' @importFrom scales "alpha"
#' @importFrom graphics "text"
#'
#' @export

measure.trace.multiple = function(traces,
                                  new_waves = NULL,
                                  waves = NULL,
                                  detections = NULL,
                                  sr = NULL,
                                  path_pdf = NULL){

  # Test if new_waves are smaller than waves - easy to enter them in wrong order
  if(!is.null(waves)) if(!all(sapply(waves, length) >= sapply(new_waves, length)))
    stop('Waves longer than new_waves! Are you sure you entered them under the correct arguments?')

  # Make data frame to save results
  measurements = data.frame()

  # Run through files
  if(!is.null(path_pdf)) pdf(path_pdf, 7, 5)
  for(i in seq_along(traces)){

    # Load wave
    if(!is.null(waves)){
      new_wave = new_waves[[i]]
      start = detections[[i]]$start
      end = detections[[i]]$end
    } else {
      start = NA
      end = NA
    }

    # Test STN
    if(!is.null(waves)){
      signal = mean(abs(new_wave@left))
      noise = mean(abs(waves[[i]][-(start:end)]@left))
    }

    # Plot
    if(!is.null(path_pdf) & !is.null(waves)){
      if(is.null(names(traces))) np = seq_along(traces) else np = names(traces)
      par(mfrow = c(2, 2))
      plot(waves[[i]])
      abline(v = c(start/waves[[i]]@samp.rate, end/waves[[i]]@samp.rate), col = 1)
      better.spectro(waves[[i]], wl = 200, ovl = 195, ylim = c(500, 4000),
                     main = np[i], mar = rep(4, 4))
      lines(traces[[i]]$time + start/waves[[i]]@samp.rate,
            traces[[i]]$fund,
            col = alpha('green', 0.3), lty = 1, lwd = 1)
      abline(h = c(mean(traces[[i]]$fund, na.rm = TRUE),
                   max(traces[[i]]$fund, na.rm = TRUE),
                   min(traces[[i]]$fund, na.rm = TRUE)), lty = 2,
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
    if(is.null(waves) & is.null(sr)){
      sr = 1
      warning('Sample rate set to 1. This will make the duration meaningless.
              Supply `sr` for meaningfull durations.')
    } else if(!is.null(waves)) sr = waves[[i]]@samp.rate
    temp = measure.trace(traces[[i]], sr = sr) # take measurements
    if(is.null(names(traces))) n = NA else n = names(traces)[i]
    if(is.null(waves)) stn = NA else stn = signal/noise
    temp = cbind(temp, data.frame(file = n, # add other info
                                  start = start,
                                  end = end,
                                  signal_to_noise = stn))
    measurements = rbind(measurements, temp) # save in main dataframe

  } # end i loop
  if(!is.null(path_pdf)) dev.off()

  # Return
  return(measurements)

} # end measure.trace.multiple
