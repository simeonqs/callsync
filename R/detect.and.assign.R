# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper
# Date started: 15-11-2022
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: This function detects calls in the chunk files and assigns them based on amplitude.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

detect.and.assign = function(ffilter_from = 1100, # from where to filter (Hz) the wave when loading
                             threshold = 0.4, # threshold for amplitude envelope when detecting call
                             return_all = T, # return all calls
                             msmooth = c(1000, 95), # smoothening settings of amplitude envelope
                             min_dur = 0.1, # minimal duration for a note to be included in seconds
                             step_size = 1/50, # step size in seconds for the alignment
                             wings = 6, # how many seconds to load before and after detection for alignment
                             all_files = NULL
                             
    ){
  
  # List files and detect recording IDs
  if(is.null(all_files)) all_files = list.files(path_chunks, pattern = '*wav', full.names = T, recursive = T)
  all_recs = all_files %>% strsplit('@') %>% sapply(`[`, 3)
  
  # Detect calls in each chunk
  message(sprintf('Running detections on %s chunks.', length(all_recs)))
  detections = lapply(all_files, function(file){
    message(sprintf('Running: %s', file))
    wave = load.wave(file, ffilter_from = ffilter_from)
    detections = call.detect.multiple(wave, 
                                      threshold = threshold,
                                      msmooth = msmooth,
                                      plot_it = F)
    return(detections)
  })  
  names(detections) = basename(all_files)
  
  # Run through unique recordings
  message('Running asignment of calls.')
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
      pdf(sprintf('%s/%s.pdf', 
                  path_calls,
                  str_remove(basename(audio_files[1]), '.wav')), 
          30*15, 14)
      par(mfrow = c(1*length(audio_files), 1), mar = c(0, 0, 0, 0), oma = c(5, 5, 1, 1))
      
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
        keep = detects$end < (length(wave@left)-wings*wave@samp.rate) & 
          detects$start > wings*wave@samp.rate
        seq_j = (1:length(detects$start))[keep]
        for(j in seq_j){
          
          # Get start and end
          start = detects$start[j]
          end = detects$end[j]
          
          # Get master chunk
          small_master = wf[start:end]
          cs = c(sum(abs(small_master@left)))
          
          # Load master
          master = wf[(start-wings*wave@samp.rate):(end+wings*wave@samp.rate)]
          step = master@samp.rate*step_size
          starts = seq(1, length(master@left)-step, step)
          s1 = sapply(starts, function(start) sum(abs(master@left[start:(start+step)])))
          
          # Run through children and calculate off-set
          seq_l = (1:length(audio_files))[-i]
          for(l in seq_l){ 
            
            # Load child
            child = wfs[[l]][(start-wings*wave@samp.rate):(end+wings*wave@samp.rate)]
            
            # Align
            starts = seq(1, length(child@left)-step, step)
            s2 = sapply(starts, function(start) sum(abs(child@left[start:(start+step)])))
            d = simple.cc(s1, s2)*step_size*wave@samp.rate
            
            # If alignment exceeds wings introduce NA in cs (-> this detection is skipped) and warn
            if(abs(d/wave@samp.rate) > wings){
              cs = c(cs, NA)
              warning(sprintf('Wings exceeded in recording %s, chunk file %s, start chunk %s and detection %s.',
                              rec, chunk_files[i], start_chunk, l))
            } else {
              
              # Add child chunk
              small_child = child[(wings*child@samp.rate+d):(length(child@left)-wings*child@samp.rate+d)]
              cs = c(cs, sum(abs(small_child@left)))
              
            } # end else
            
          } # end l loop (children)
          
          # Test if master was the loudest
          if(any(is.na(cs))) next 
          if(cs[1] == max(cs)){
            abline(v = start/wave@samp.rate, lty = 2, col = 'green', lwd = 2)
            abline(v = end/wave@samp.rate, lty = 2, col = 'green', lwd = 2)
            writeWave(wave[(start-0.1*wave@samp.rate):(end+0.1*wave@samp.rate)],
                      file = sprintf('%s/%s@%s-%s.wav',
                                     path_calls,
                                     str_remove(basename(audio_files[i]), '.wav'),
                                     start, 
                                     end),
                      extensible = F)
          } 
          
        } # end j loop (starts)
        
      } # end i loop (files)
      
      # Close PDF
      axis(1, at = seq(0, 15*60, 15), format(seq(as.POSIXct('2013-01-01 00:00:00', tz = 'GMT'), 
                                                 length.out = 15*4+1, by = '15 sec'), '%M:%S'))
      dev.off()
      
    } # end start_chunk
    
  } # end folder loop
  
  # Message
  message('All done!')
  
} # end detect.and.assign