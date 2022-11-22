# *callsync*: sychronous analysis of multiple microphones

**Note**: This package is still under construction. If you want to test it, feel free to contact me (<simeonqs@hotmail.com>) with any questions. 

`callsync` is an R package intended for users that use collar or backpack microphones and want to analyse the resulting recordings. It allows users to create a pipeline from raw recordings until final feature vectors, but specific functions can also be replaced by alternatives. 

The main features are:

1. **alignment** and partitioning of drifting microphones using signal compression and cross correlation
2. call **detection** using an amplitude envelope
3. fine-scale alignment and call **assignment** across recordings using cross correlation and energy content
4. fundamental frequency **tracing**
5. **analysis** of the resulting traces and wav clips

![*Flowchart for the `callsync` package.*](flowchart.png)

The package also offers:

- flexible spectrograms
- spectrographic cross correlation
- analysis of amplitude and frequency modulation

To install and load the package run:

```
install.packages('devtools')
library(devtools)
devtools::install_github('simeonqs/callsync')
library(callsync)
```

