#' @title align
#'
#' @description Aligns multiple recordings (.wav files). It assumes all microphones are within recording range
#' of each other most of the time.
#'
#' @param chunk_size numeric, duration in minutes of the chunks to output. Default is `15`.
#' @param step_size numeric, duration in seconds of the bins for signal compression before cross correlation.
#' Default is `0.5`.
#' @param all_files character vector, paths to all raw recordings to consider. If `NULL` files are listed
#' based on the argument `path_recordings`.
#' @param path_recordings character, the path where the raw recordings are stored. Can be nested in folders,
#' in this case provide the top-level folder.
#' @param path_chunks character, the path where aligned chunks should be stored.
#' @param keys_id character vector of length 2. The characters before and after the unique ID of the
#' individual or microphone. This can be in the file name or in the folder structure. E.g., if the path to the
#' recording is `../data/week_1/recording_mic1.wav` the keys would be `c('recording', '.wav')`.
#' @param keys_rec character vector of length 2. The characters before and after the unique ID of the
#' recording. This can be in the file name or in the folder structure. E.g., if the path to the recording
#' is `../data/week_1/recording_mic1.wav` the keys would be `c('data/', '_mic')`.
#' @param blank numeric, the duration in minutes to be discarded at the beginning and end of the recording.
#' @param wing numeric, the duration in minutes to load before and after each chunk to improve alignment. This
#' is not saved with the aligned chunk.
#' @param save_pdf logical, if `TRUE` a pdf is saved with a page per chunk that shows all the aligned
#' recordings.
#'
#' @return saves all the aligned chunks in the location specific by `path_chunks`.
#'
#' @export
#'
#' @importFrom tuneR "readWave"
#' @importFrom tuneR "writeWave"

align = function(chunk_size = 15, # duration of output in minutes
                 step_size = 0.5, # step size in seconds
                 all_files = NULL, # alternatively a vector of full file names
                 path_recordings = NULL, # the path where folders are stored
                 path_chunks = NULL, # where to store the chunks and optional pdf
                 keys_id = NULL,
                 keys_rec = NULL,
                 blank = 15, # duration of blank before/after taking chunks in minutes
                 wing = 10, # how much to load extra for alignment in minutes
                 save_pdf = FALSE # if T saves a single PDF per folder with 1 chunk per page
){

  # Run checks
  if(wing > blank) stop('Wing cannot be greater than blank.')

  # List files and detect recording IDs
  if(is.null(all_files)) all_files = list.files(path_recordings, full.names = T, recursive = T)
  all_recs = all_files %>% strsplit(keys_rec[1]) %>% sapply(`[`, 2) %>%
    strsplit(keys_rec[2]) %>% sapply(`[`, 1)

  # Create list to save chunks
  if(is.null(path_chunks)) chunk_list = list()

  # Run through unique recordings
  for(rec in unique(all_recs)){

    # List files
    files = all_files[str_detect(all_files, rec)]

    # Open PDF - if needed
    if(save_pdf){
      pdf(sprintf('%s/%s.pdf', path_chunks, str_remove(basename(files[1]), '.wav')), 15, 10)
      par(mfrow = c(length(files), 1), mar = c(0, 0, 0, 0), oma = c(5, 1, 1, 1))
    }

    # Check for the min duration
    sizes = files %>% lapply(file.info) %>% sapply(function(x) x$size) # load file size for all files
    wave = readWave(files[which(sizes == min(sizes))]) # load the smallest file (this must also be shortest)
    ## retrieve min duration: take the floor to get the maximal number of chunks that fits, then multiply by
    ## the chunk size again to get the min duration back in minutes
    min_duration = floor(length(wave@left) / wave@samp.rate / 60 / chunk_size) * chunk_size

    # Run through chunks
    chunk_seq = seq(blank, # start after the blank
                    min_duration-blank-chunk_size, # until minimum duration - blank and chunk
                    chunk_size) # by chunk steps
    message(sprintf('Running recording: %s. Running %s chunks with start times: ', rec, length(chunk_seq)))
    for(chunk in chunk_seq){
      message(chunk)

      # Load master
      master = readWave(files[1], from = chunk - wing, to = chunk + chunk_size + wing, units = 'minutes')

      # Sum the sound per step
      step = master@samp.rate*step_size
      starts = seq(1, length(master@left)-step, step)
      s1 = sapply(starts, function(start) sum(abs(master@left[start:(start+step)])))

      # Plot - if needed
      if(save_pdf){
        ## compute the max of the y-axis
        max_y = 2^wave@bit/2 * step_size * wave@samp.rate / 2
        ## plot
        times = starts/step/60*step_size
        plot(times, s1,
             type = 'l', xlim = c(-wing, max(times) + wing), ylim = c(0, max_y), xaxt = 'n', yaxt = 'n',
             main = chunk)
      }

      # Save master
      id =  files[1] %>% strsplit(keys_id[1]) %>% sapply(`[`, 2) %>% strsplit(keys_id[2]) %>% sapply(`[`, 1)
      new_master = master[(wing*60*master@samp.rate):(length(master@left)-wing*60*master@samp.rate)]
      if(!is.null(path_chunks)){
        writeWave(new_master,
                  sprintf('%s/%s@%s@%s@%s.wav',
                          path_chunks, str_remove(basename(files[1]),'.wav'), id, rec, chunk),
                  extensible = F)
      } else chunk_list[[sprintf(paste(c(str_remove(basename(files[1]),'.wav'), id, rec, chunk),
                                       collapse = '@'))]] = new_master


      # Run through children and calculate off-set
      for(i in 2:length(files)){

        # Load child
        child = readWave(files[i], from = chunk - wing, to = chunk + chunk_size + wing, units = 'minutes')

        # Align
        starts = seq(1, length(child@left)-step, step)
        s2 = sapply(starts, function(start) sum(abs(child@left[start:(start+step)])))
        d = simple.cc(s1, s2)*step_size

        # Plot
        if(save_pdf){
          times = starts/step/60*step_size - d/60
          plot(times, s2, type = 'l', ylim = c(0, max_y), xlim = c(-wing, max(times + d/60) + wing),
               xaxt = 'n', yaxt = 'n')
        }

        # Save child
        id =  files[i] %>% strsplit(keys_id[1]) %>% sapply(`[`, 2) %>% strsplit(keys_id[2]) %>% sapply(`[`, 1)
        new_child = child[(wing*60*child@samp.rate + d*child@samp.rate):
                            (length(child@left)-wing*60*child@samp.rate + d*child@samp.rate)]
        if(!is.null(path_chunks)){
          writeWave(new_child,
                    sprintf('%s/%s@%s@%s@%s.wav',
                            path_chunks, str_remove(basename(files[i]),'.wav'), id, rec, chunk),
                    extensible = F)
        } else chunk_list[[sprintf(paste(c(str_remove(basename(files[i]),'.wav'), id, rec, chunk),
                                         collapse = '@'))]] = new_child

      } # end i loop

      # Add axis
      if(save_pdf){
        axis(1)
        mtext('time [m]', 1, 3)
      }

    } # end chunk loop

    # Save PDF
    if(save_pdf) dev.off()

  } # end folder loop

  # Return if not saved to file
  if(is.null(path_chunks)) return(chunk_list)

} # end align
