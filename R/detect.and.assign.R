#' @title trace.fund
#'
#' @description Traces the fundamental frequency from a wave object. Also applies smoothening to trace.
#'
#' @param all_files character vector or `NULL`. Character vector should contain all the paths to the raw
#' recordings that should be considered.
#' @param path_chunks character, path to where to store the results.
#' @param ffilter_from numeric, frequency in Hz for the high-pass filter.
#' @param threshold numeric, threshold (fraction of the maximum) for amplitude envelope when detecting call.
#' @param msmooth used as argument for the `seewave::env` function. *A vector of length 2 to smooth the
#' amplitude envelope with a mean sliding window. The first component is the window length (in number of
#' points). The second component is the overlap between successive windows (in \%).* Default is `c(500, 95)`.
#' @param plot_it  if `TRUE`, returns three-panel plot of wave form, envelope and spectrogram to current
#' @param min_dur numeric, the minimal duration in seconds for a detection to be saved. Default is `0.1`.
#' @param max_dur numeric, the maximal duration in seconds for a detection to be saved. Default is `0.3`.
#' @param step_size numeric, duration in seconds of the bins for signal compression before cross correlation.
#' Default is `0.01`.
#' @param wing numeric, the duration in minutes to load before and after each chunk to improve alignment. This
#' is not saved with the aligned chunk.
#' @param save_files logical, if `TRUE` the files are stored in the `path_chunks` location. Results are also
#' returned.
#'
#' @return Returns a data frame with start = start time in samples and end = end time in samples for each
#' detection.
#'
#' @export
#'
#' @importFrom seewave "env"
#' @importFrom tuneR "readWave"
#' @importFrom tuneR "writeWave"

detect.and.assign = function(all_files = NULL,
                             path_chunks = NULL,
                             ffilter_from = 1100,
                             threshold = 0.4,
                             msmooth = c(1000, 95),
                             min_dur = 0.1,
                             max_dur = 0.30,
                             step_size = 0.01,
                             wing = 6,
                             save_files = TRUE

){

  # List files and detect recording IDs
  if(is.null(all_files)) all_files = list.files(path_chunks, pattern = '*wav', full.names = T, recursive = T)
  # all_files = all_files[str_detect(all_files, '@180')]
  all_recs = all_files %>% strsplit('@') %>% sapply(`[`, 3)

  # Detect calls in each chunk
  message(sprintf('Running detections on %s chunks.', length(all_recs)))
  detections = lapply(all_files, function(file){
    message(sprintf('Running: %s', file))
    wave = load.wave(file, ffilter_from = ffilter_from)
    detections = call.detect.multiple(wave,
                                      threshold = threshold,
                                      msmooth = msmooth,
                                      min_dur = min_dur,
                                      max_dur = max_dur,
                                      plot_it = F)
    return(detections)
  })
  names(detections) = basename(all_files)

  # Create dataframe to store detections
  detec_saver = data.frame()

  # Run through unique recordings
  message('Running assignment of calls.')
  for(rec in unique(all_recs)){

    # Subset for recording
    chunk_files = all_files[str_detect(all_files, rec)]

    # Get the chunk _time.wav keys
    starts_chunks = chunk_files %>% str_split('@') %>% unlist %>% unique
    starts_chunks = starts_chunks[str_detect(starts_chunks, '.wav')]

    # Run through major chunks
    for(start_chunk in starts_chunks){

      # Message
      message(sprintf('Running _%s...', start_chunk))

      # List files and load
      audio_files = chunk_files[str_detect(chunk_files, start_chunk)]

      # Test chunk
      waves = lapply(audio_files, load.wave, from = 0, to = Inf)
      wfs = lapply(audio_files, load.wave, from = 0, to = Inf, ffilter_from = ffilter_from)

      # Open PDF
      if(save_files){
        pdf(sprintf('%s/%s.pdf',
                    path_calls,
                    str_remove(basename(audio_files[1]), '.wav')),
            30*15, 14)
        par(mfrow = c(1*length(audio_files), 1), mar = c(0, 0, 0, 0), oma = c(5, 5, 1, 1))
      }

      # Run through files
      for(i in 1:length(audio_files)){

        # Load wave
        wave = waves[[i]]
        wf = wfs[[i]]

        # Subset the detections
        detects = detections[[basename(audio_files[i])]]

        # Plot wave
        plot(wf, xaxs = 'i', xaxt = 'n', nr = 15*2500)

        # Test if any detections else skip
        if(nrow(detects) == 0) next

        # Run through detections and select
        ## do not consider start and end times that cannot fit a wing
        keep = detects$end < (length(wave@left)-wing*wave@samp.rate) &
          detects$start > wing*wave@samp.rate
        seq_j = (1:length(detects$start))[keep]
        for(j in seq_j){

          # Get start and end
          start = detects$start[j]
          end = detects$end[j]

          # Get master chunk
          small_master = wf[start:end]
          cs = c(sum(abs(small_master@left)))

          # Load master
          master = wf[(start-wing*wave@samp.rate):(end+wing*wave@samp.rate)]
          step = master@samp.rate*step_size
          starts = seq(1, length(master@left)-step, step)
          s1 = sapply(starts, function(start) sum(abs(master@left[start:(start+step)])))

          # Run through children and calculate off-set
          seq_l = (1:length(audio_files))[-i]
          for(l in seq_l){

            # Load child
            child = wfs[[l]][(start-wing*wave@samp.rate):(end+wing*wave@samp.rate)]

            # Align
            starts = seq(1, length(child@left)-step, step)
            s2 = sapply(starts, function(start) sum(abs(child@left[start:(start+step)])))
            d = simple.cc(s1, s2)*step_size*wave@samp.rate

            # If alignment exceeds wing introduce NA in cs (-> this detection is skipped) and warn
            if(abs(d/wave@samp.rate) > wing){
              cs = c(cs, NA)
              warning(sprintf('wing exceeded in recording %s, chunk file %s, start chunk %s and detection %s.',
                              rec, chunk_files[i], start_chunk, l))
            } else {

              # Add child chunk
              small_child = child[(wing*child@samp.rate+d):(length(child@left)-wing*child@samp.rate+d)]
              cs = c(cs, sum(abs(small_child@left)))

            } # end else

          } # end l loop (children)

          # Test if master was the loudest
          if(any(is.na(cs))) next
          if(cs[1] == max(cs)){
            if(save_files){
              abline(v = start/wave@samp.rate, lty = 2, col = 'green', lwd = 2)
              abline(v = end/wave@samp.rate, lty = 2, col = 'green', lwd = 2)
              rect(xleft = start/wave@samp.rate, xright = end/wave@samp.rate,
                   ybottom = par("usr")[3], ytop = par("usr")[4],
                   border = NA, col = alpha('green', 0.3))
              writeWave(wave[(start-0.1*wave@samp.rate):(end+0.1*wave@samp.rate)],
                        file = sprintf('%s/%s@%s-%s.wav',
                                       path_calls,
                                       str_remove(basename(audio_files[i]), '.wav'),
                                       start,
                                       end),
                        extensible = F)
            }

            # Save for output as well
            detec_saver = rbind(detec_saver, data.frame(file = str_remove(basename(audio_files[i]), '.wav'),
                                                        start = start,
                                                        end = end))

          } # end test if master was the loudest

        } # end j loop (starts)

      } # end i loop (files)

      # Close PDF
      axis(1, at = seq(0, 15*60, 15), format(seq(as.POSIXct('2013-01-01 00:00:00', tz = 'GMT'),
                                                 length.out = 15*4+1, by = '15 sec'), '%M:%S'))
      dev.off()

    } # end start_chunk

  } # end folder loop

  # Return
  return(detec_saver)

  # Message
  message('All done!')

} # end detect.and.assign
