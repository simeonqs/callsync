# *callsync*: sychronous analysis of multiple microphones

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/callsync)](https://cran.r-project.org/package=callsync)
[![Dependencies](https://tinyverse.netlify.com/badge/callsync)](https://cran.r-project.org/package=callsync) 
[![Project Status: Active  The project is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.en.html) 

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
- loading selection tables from Raven and Audacity
- calculating performance of the detection and assignment functions

The tested version can be installed from CRAN. To install and load the developmental version run:

```
install.packages('devtools')
library(devtools)
devtools::install_github('simeonqs/callsync')
library(callsync)
```

Feel free to contact me (<simeonqs@hotmail.com>) with any questions, suggestions or requests.
