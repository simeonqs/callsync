# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: methods paper
# Date started: 20-10-2021
# Date last modified: 16-11-2022
# Author: Simeon Q. Smeele
# Description: Loads wave from selection table and audio file. Filters, checks for clipping.
# This version has no filter.
# This version makes from and to optional and includes an optional filter.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(tuneR)
require(seewave)

load.wave = function(path_audio_file,
                     from = 0,
                     to = Inf,
                     ffilter_from = NULL){

  wave = readWave(path_audio_file,
                  from = as.numeric(from),
                  to = as.numeric(to),
                  units = 'seconds')

  if(!is.null(ffilter_from)) wave = ffilter(wave, from = ffilter_from, output = 'Wave')

  return(wave)

}
