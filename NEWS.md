# callsync 0.0.0

* First release.

# callsync 0.0.1

* Updated description and size to accommodate comments from CRAN review. 

# callsync 0.0.2

* Updated description to accommodate comments from CRAN review. 

# callsync 0.0.3

* Fixed all T/F to TRUE/FALSE.
* Included `on.exit()` for functions that modify `par()`. 

# callsync 0.0.4

* Fixed a bug. Previous version for call.assign would include chunk `120` when running chunk `20` because of 
  how regex was formulated. 

# callsync 0.0.5 

* Added error messages.
* Fixed bug with flipping y axis in `align` if only two tracks were aligned. 

# callsync 0.0.6

* Added CITATION file for the preprint. 

# callsync 0.0.7

* `calc.fm` now also output the number of peaks (np). 
* Fixed bug in `call.detect.multiple`. If save_extra was used, it sometimes stored clips outside the range
  of the wave file (if detections were close to start or end). Now these start and end times are replaced 
  by 0 and the length of the wave file.
* Fixed bug in `detect.and.assign`. If detections were too close to the bounds, the wing went outside the 
  chunk. These detections are now removed, since they cannot be aligned. 
  
# callsync 0.1.0

* Adding additional functions and modifying/adding options to existing functions. All
  original functions should still work without changing any settings. 
  
# callsync 0.2.0

* Adding vignette.
* Minor updates to descriptions.
* Adding units to column titles align_log.

# callsync 0.2.1

* Removing files from inst/extdata, now downloading from GitHub instead. 
* Speeding up testing and code examples. 

# callsync 0.2.2

* Creating specific error if ovl >= wl in better.spectro. Also updating default for ovl (=wl/2), this might impact how spectrograms are rendered if you used the default settings (wl = 512, ovl = 450). 

# callsync 0.2.2

* Updating reference to now published Ecology and Evolution version of the paper (before it was BioRxiv).
