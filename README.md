# callsync: recording alignment, call detection and assignment, audio analysis

**Note**: This package is still under construction. If you want to test it, feel free to contact me (<simeonqs@hotmail.com>) with any questions. 

`callsync` is intended for users that use collar or backpack microphones and want to analyse the resulting recordings. It allows users to create a pipeline from raw recordings until final feature vectors, but specific functions can also be replaced by alternatives. 

The main features are:

- alignment and partitioning of drifting microphones using signal compression and cross correlation
- call detection using an amplitude envelope
- fine-scale alignment and call assignment across recordings using cross correlation and energy content
- fundamental frequency tracing and analysis
- filtering of noise
- spectrographic cross correlation

The package also offers:

- flexible spectrograms

To install and load the package run:

```
install.packages('devtools')
library(devtools)
devtools::install_github('simeonqs/callsync')
library(callsync)
```

