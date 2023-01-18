#' @title align
#'
#' @description Aligns multiple recordings (.wav files). It assumes all microphones are within recording range
#' of each other most of the time.
#'
#' @details There are two ways to tell the function where the files are. You can either compile a character
#' vector of pathnames yourself and enter this under `all_files` or you can give a single character path to
#' `path_recordings`. You need to make sure that there is an identifier by which to group the recordings and
#' an identifier for each individual or microphone in the path. This can either be a in the folder structure
#' or in the file names themselves. The align function will align all individuals per recording id (e.g.,
#' date). These identifiers are found using regexp, so mostly you can use the first few characters before
#' and after them (see examples in the argument descriptions). The function loads chunks of the recordings,
#' sums the absolute amplitude per bin and runs cross correlation to find alignment relative to the first
#' recording. The aligned chunks are then saved.
#'
#' @param chunk_size numeric, duration in minutes of the chunks to output. Default is `15`.
#' @param step_size numeric, duration in seconds of the bins for signal compression before cross correlation.
#' Default is `0.5`.
#' @param all_files character vector, paths to all raw recordings to consider. If `NULL` files are listed
#' based on the argument `path_recordings`.
#' @param path_recordings character, the path where the raw recordings are stored. Can be nested in folders,
#' in this case provide the top-level folder.
#' @param path_chunks character, the path where aligned chunks should be stored.
#' @param chunk_seq numeric vector or `NULL`. If supplied only these chunks are rerun.
#' @param keys_id character vector of length 2. The characters before and after the unique ID of the
#' individual or microphone. This can be in the file name or in the folder structure. E.g., if the path to the
#' recording is `../data/week_1/recording_mic1.wav` the keys would be `c('recording', '.wav')`.
#' @param keys_rec character vector of length 2. The characters before and after the unique ID of the
#' recording. This can be in the file name or in the folder structure. E.g., if the path to the recording
#' is `../data/week_1/recording_mic1.wav` the keys would be `c('data/', '_mic')`.
#' @param blank numeric, the duration in minutes to be discarded at the beginning and end of the recording.
#' @param wing numeric, the duration in minutes to load before and after each chunk to improve alignment. This
#' is not saved with the aligned chunk.
#' @param ffilter_from numeric, frequency in Hz for the high-pass filter.
#' @param down_sample numeric, the sample rate for down-sampling. If `NULL` no down-sampling is done.
#' @param save_pdf logical, if `TRUE` a pdf is saved with a page per chunk that shows all the aligned
#' recordings.
#' @param save_log logical, if `TRUE` a csv file with all alignment times is saved in path_chunks.
#' @param quiet logical, if `TRUE` no messages are printed.
#'
#' @return saves all the aligned chunks in the location specific by `path_chunks`.
#'
#' @export
#'
#' @importFrom tuneR "readWave"
#' @importFrom tuneR "writeWave"
#' @importFrom seewave "resamp"
#' @importFrom stringr "str_detect"
#' @importFrom grDevices "pdf"
#' @importFrom graphics "par"
#' @importFrom graphics "axis"
#' @importFrom graphics "mtext"
#' @importFrom grDevices "dev.off"
#' @importFrom stats "var"
#' @importFrom utils "write.csv2"

align = function(chunk_size = 15,
                 step_size = 0.5,
                 all_files = NULL,
                 path_recordings = NULL,
                 path_chunks = NULL,
                 chunk_seq = NULL,
                 keys_id = NULL,
                 keys_rec = NULL,
                 blank = 15,
                 wing = 10,
                 ffilter_from = NULL,
                 down_sample = NULL,
                 save_pdf = FALSE,
                 save_log = FALSE,
                 quiet = FALSE
){

  # Run checks
  if(wing > blank) stop('Wing cannot be greater than blank.')
  if(is.null(all_files))
    if(length(list.files(path_recordings, pattern = '*WAV', full.names = TRUE, recursive = TRUE)) > 0)
      warning('Detected files with extension .WAV. Only files with .wav will be run.')

  # List files and detect recording IDs
  if(is.null(all_files)) all_files = list.files(path_recordings, pattern = '*wav',
                                                full.names = TRUE, recursive = TRUE)
  all_recs = all_files |> strsplit(keys_rec[1]) |> sapply(`[`, 2) |>
    strsplit(keys_rec[2]) |> sapply(`[`, 1)

  # Create list to save chunks
  if(is.null(path_chunks)) chunk_list = list()

  # Optionally create list to save alignment details
  if(save_log) align_log = data.frame()

  # Run through unique recordings
  for(rec in unique(all_recs)){

    # List files
    files = all_files[str_detect(all_files, rec)]

    # Check if sample rate is all the same
    minis = lapply(files, load.wave, from = 0, to = 0.1)
    if(!is.null(down_sample)) minis = lapply(minis, function(x){
      if(x@samp.rate == down_sample) return(x) else
        return(seewave::resamp(x, g = down_sample, output = 'Wave'))
    })
    srs = sapply(minis, function(x) x@samp.rate)
    if(!stats::var(srs) == 0)
      warning(sprintf('Not all sample rates are equal. Check your raw data for recording %s.', rec))

    # Open PDF - if needed
    if(save_pdf){
      oldpar = par(no.readonly = TRUE)
      on.exit(par(oldpar))
      pdf(sprintf('%s/%s.pdf', path_chunks, str_remove(basename(files[1]), '.wav')), 9, length(files)/2)
      par(mfrow = c(length(files), 1), mar = c(0, 0, 0, 0), oma = c(5, 1, 3, 1))
    }

    # Check for the min duration
    sizes = files |> lapply(file.info) |> sapply(function(x) x$size) # load file size for all files
    wave = readWave(files[which(sizes == min(sizes))][1]) # load the smallest file (this must also shortest)
    if(!is.null(down_sample)) if(wave@samp.rate != down_sample)
      wave = seewave::resamp(wave, g = down_sample, output = 'Wave')
    ## retrieve min duration: take the floor to get the maximal number of chunks that fits, then multiply by
    ## the chunk size again to get the min duration back in minutes
    min_duration = floor(length(wave@left) / wave@samp.rate / 60 / chunk_size) * chunk_size

    # Run through chunks
    if(is.null(chunk_seq))
      chunk_seq = seq(blank, # start after the blank
                      min_duration-blank-chunk_size, # until minimum duration - blank and chunk
                      chunk_size) # by chunk steps
    if(!quiet) message(sprintf('Running %s recordings with id: %s. Running %s chunks with start times: ',
                               length(files), rec, length(chunk_seq)))
    for(chunk in chunk_seq){
      if(!quiet) message(chunk)

      # Load master
      master = readWave(files[1], from = chunk - wing, to = chunk + chunk_size + wing, units = 'minutes')
      if(!is.null(down_sample)) if(master@samp.rate != down_sample)
        master = seewave::resamp(master, g = down_sample, output = 'Wave')

      # Optionally save alignment log
      if(save_log) align_log = rbind(align_log, data.frame(rec = rec, file = files[1], chunk = chunk,
                                                           from = chunk,
                                                           to = chunk + chunk_size,
                                                           offset = 0))

      # Sum the sound per step
      step = master@samp.rate*step_size
      starts = seq(1, length(master@left)-step, step)
      if(!is.null(ffilter_from)) mf = ffilter(master, from = ffilter_from, output = 'Wave') else mf = master
      s1 = sapply(starts, function(start) sum(abs(mf@left[start:(start+step)])))

      # Plot - if needed
      if(save_pdf){
        ## plot
        times = starts/step/60*step_size
        plot(times, s1,
             type = 'l', xlim = c(-wing/2, max(times) + wing/2), xaxt = 'n', yaxt = 'n',
             main = '', col = '#3a586e')
        mtext(chunk, line = 1)
      }

      # Save master
      id =  files[1] |> strsplit(keys_id[1]) |> sapply(`[`, 2) |> strsplit(keys_id[2]) |> sapply(`[`, 1)
      new_master = master[(wing*60*master@samp.rate):(length(master@left)-wing*60*master@samp.rate)]
      if(!is.null(path_chunks)){
        writeWave(new_master,
                  sprintf('%s/%s@%s@%s@%s.wav',
                          path_chunks, str_remove(basename(files[1]),'.wav'), id, rec, chunk),
                  extensible = FALSE)
      } else chunk_list[[sprintf(paste(c(str_remove(basename(files[1]),'.wav'), id, rec, chunk),
                                       collapse = '@'))]] = new_master


      # Run through children and calculate off-set
      for(i in 2:length(files)){

        # Load child
        child = readWave(files[i], from = chunk - wing, to = chunk + chunk_size + wing, units = 'minutes')
        if(!is.null(down_sample)) if(child@samp.rate != down_sample)
          child = seewave::resamp(child, g = down_sample, output = 'Wave')

        # Align
        starts = seq(1, length(child@left)-step, step)
        if(!is.null(ffilter_from)) cf = ffilter(child, from = ffilter_from, output = 'Wave') else cf = child
        s2 = sapply(starts, function(start) sum(abs(cf@left[start:(start+step)])))
        d = simple.cc(s1, s2)*step_size
        if(abs(d) > wing*60){
          warning(paste0('Alignment adjustment exceeds wing in chunk ', chunk, ' of recording ',
                         files[i], '. Make sure the wing is large enough. Otherwise alignment might not be ',
                         'possible with the current settings. Current chunk will be stored without ',
                         'alignment.'))
          d = 0
        }

        # Optionally save alignment log
        if(save_log) align_log = rbind(align_log, data.frame(rec = rec, file = files[i], chunk = chunk,
                                                             from = chunk + d,
                                                             to = chunk + chunk_size + d,
                                                             offset = d))

        # Plot
        if(save_pdf){
          times = starts/step/60*step_size - d/60
          plot(times, s2, type = 'l', xlim = c(-wing/2, max(times + d/60) + wing/2),
               xaxt = 'n', yaxt = 'n', col = '#3a586e')
        }

        # Save child
        id =  files[i] |> strsplit(keys_id[1]) |> sapply(`[`, 2) |> strsplit(keys_id[2]) |> sapply(`[`, 1)
        new_child = child[(wing*60*child@samp.rate + d*child@samp.rate):
                            (length(child@left)-wing*60*child@samp.rate + d*child@samp.rate)]
        if(!is.null(path_chunks)){
          writeWave(new_child,
                    sprintf('%s/%s@%s@%s@%s.wav',
                            path_chunks, str_remove(basename(files[i]),'.wav'), id, rec, chunk),
                    extensible = FALSE)
        } else chunk_list[[sprintf(paste(c(str_remove(basename(files[i]),'.wav'), id, rec, chunk),
                                         collapse = '@'))]] = new_child

      } # end i loop

      # Add axis
      if(save_pdf){
        axis(1, cex.axis = 1.5)
        mtext('time [m]', 1, 3, cex = 1)
      }

    } # end chunk loop

    # Save PDF
    if(save_pdf) dev.off()

  } # end folder loop

  # Optionally save alignment log
  if(save_log) utils::write.csv2(align_log, sprintf('%s/align_log.csv', path_chunks), row.names = FALSE)

  # Return if not saved to file
  if(is.null(path_chunks)) return(chunk_list)

} # end align
