# *callsync*: sychronous analysis of multiple microphones

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/callsync)](https://cran.r-project.org/package=callsync)
[![Project Status: Active  The project is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.en.html) 
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/callsync)](https://cranlogs.r-pkg.org/badges/grand-total/callsync)

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

A vignette going trough the main functions can be found [here](vignettes/examples_main_functions_callsync.Rmd).

# Requirements

R version 4.1.0 or later. Earlier versions might work if you replace the `|>` function with `%>%` and load the package `tidyverse`.

# Install

The tested version can be installed from CRAN:

```
install.packages('callsync')
library(callsync)
```

To install and load the developmental version run:

```
install.packages('devtools')
library(devtools)
devtools::install_github('simeonqs/callsync')
library(callsync)
```

# Maintenance and contact

The package should be fully functional, and extra features are actively being developed. Feel free to contact me (<simeonqs@hotmail.com>) with any questions, suggestions or requests.

# Examples and citation

If you want a framework that uses all the main functions you can read [our preprint](https://doi.org/10.1101/2023.02.07.527470) and can clone [this repository](https://github.com/simeonqs/callsync_an_R_package_for_alignment_and_analysis_of_multi-microphone_animal_recordings). To cite the package, you can use the following citation:

```
Smeele, S. Q., Tyndel, S. A., Klump, B. C., Alarcon-Nieto, G. & Aplin, L. M. (2023). callsync: an R package for alignment and analysis of multi-microphone animal recordings. BioRxiv. <https://doi.org/10.1101/2023.02.07.527470>
```
