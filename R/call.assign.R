#' @title call.assign
#'
#' @description Assigns calls from a detection table. Or rather removes calls that are not the loudest and
#' returns the cleaned detection table. Uses fine alignment and energy content.
#'
#' @param all_files character vector, should contain all the paths to the raw recordings that should be
#' considered. If `NULL` files are loaded from `path_chunks`.
#' @param path_calls character, path to where to store the results.
#' @param detections data frame with start = start time in samples and end = end time in samples for each
#' detection.
#' @param save_files logical, if `TRUE` the files are stored in the `path_chunks` location. Results are also
#' returned.
#' @param ffilter_from numeric, frequency in Hz for the high-pass filter.
#' @param wing numeric, the duration in minutes to load before and after each chunk to improve alignment. This
#' is not saved with the aligned chunk.
#' @param step_size numeric, duration in seconds of the bins for signal compression before cross correlation.
#' Default is `0.01`.
#' @param assign_fraq numeric between 0 and 1, how much louder does the focal needs to be than the second
#' loudest track to be accepted. Default is `0.05` and accepts if the focal is just 0.05 louder.
#' @param save_wing numeric, how much extra to export before and after a detection to make sure the whole call
#' is included in seconds. Default is `0.1`.
#' @param quiet logical, if `TRUE` no messages are printed.
#'
#' @return Returns a data frame with file = file name, start = start time in samples and end = end time in
#' samples for each detection.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' files = system.file("extdata", "", package = "callsync")
#' all_files = list.files(files, '*chunk*', full.names = T)
#' detections = lapply(all_files, function(file){
#'   wave = load.wave(file, ffilter_from = 1100)
#'   detections = call.detect.multiple(wave, plot_it = F)
#'   return(detections)
#' })
#' names(detections) = basename(all_files)
#' ca = call.assign(all_files = all_files,
#'                  detections = detections,
#'                  quiet = TRUE,
#'                  save_files = FALSE)
#'
#' @export
#'
#' @importFrom tuneR "readWave"
#' @importFrom tuneR "writeWave"
#' @importFrom stringr "str_detect"
#' @importFrom graphics "abline"
#' @importFrom graphics "rect"
#' @import tuneR

call.assign = function(all_files = NULL,
                       detections = NULL,
                       save_files = TRUE,
                       path_calls = NULL,
                       ffilter_from = 1100,
                       wing = 5,
                       step_size = 0.01,
                       assign_fraq = 0.05,
                       save_wing = 0.1,
                       quiet = FALSE){

  # Detect recording IDs
  all_recs = all_files |> strsplit('@') |> sapply(`[`, 3)

  # Create data frame to store retained detections
  detec_saver = data.frame()

  # Run through unique recordings
  if(!quiet) message('Running assignment of calls.')
  for(rec in unique(all_recs)){

    # Subset for recording
    chunk_files = all_files[str_detect(all_files, rec)]

    # Get the chunk _time.wav keys
    starts_chunks = chunk_files |> strsplit('@') |> unlist() |> unique()
    starts_chunks = starts_chunks[str_detect(starts_chunks, '.wav')]

    # Run through major chunks
    for(start_chunk in starts_chunks){

      # Message
      if(!quiet) message(sprintf('Running _%s...', start_chunk))

      # List files and load
      stch = sprintf('@%s', start_chunk)
      audio_files = chunk_files[str_detect(chunk_files, stch)]

      # Test chunk
      waves = lapply(audio_files, load.wave, from = 0, to = Inf)
      wfs = lapply(audio_files, load.wave, from = 0, to = Inf, ffilter_from = ffilter_from)

      # Open PDF
      if(save_files){
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        pdf(sprintf('%s/%s.pdf',
                    path_calls,
                    str_remove(basename(audio_files[1]), '.wav')),
            30*15, 14)
        par(mfrow = c(1*length(audio_files), 1), mar = c(0, 0, 0, 0), oma = c(5, 5, 1, 1))
      }

      # Run through files
      for(i in seq_along(audio_files)){

        # Load wave
        wave = waves[[i]]
        wf = wfs[[i]]

        # Subset the detections
        detects = detections[[basename(audio_files[i])]]
        detects = detects[detects$start - wing*wave@samp.rate > 1 &
                            detects$end + wing*wave@samp.rate < length(wf@left),]

        # Plot wave
        plot(wf, xaxs = 'i', xaxt = 'n', nr = 15*2500)
        text(1, 0.5 * max(wf@left), labels = basename(audio_files[i]), adj = 0)

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
          second_loudest = sort(cs, decreasing = TRUE)[2]
          if(cs[1] == max(cs) & cs[1] * (1-assign_fraq) > second_loudest){
            if(save_files){
              graphics::rect(xleft = (start-save_wing)/wave@samp.rate,
                             xright = (end+save_wing)/wave@samp.rate,
                             ybottom = par("usr")[3], ytop = par("usr")[4],
                             border = NA, col = alpha('#3a586e', 0.5))
              abline(v = (start-save_wing)/wave@samp.rate, lty = 2, col = '#3a586e', lwd = 3)
              abline(v = (end+save_wing)/wave@samp.rate, lty = 2, col = '#3a586e', lwd = 3)
              writeWave(wave[(start-save_wing*wave@samp.rate):(end+save_wing*wave@samp.rate)],
                        filename = sprintf('%s/%s@%s-%s.wav',
                                           path_calls,
                                           str_remove(basename(audio_files[i]), '.wav'),
                                           start,
                                           end),
                        extensible = FALSE)
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
  if(!save_files) return(detec_saver)

} # end call.assign
