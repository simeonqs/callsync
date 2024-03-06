\docType{package}
\name{callsync}
\alias{callsync}
\alias{callsync-package}
\title{callsync: sychronous analysis of multiple microphones}

\description{
Intended to analyse recordings from multiple microphones (e.g., backpack
  microphones in captive setting). It allows users to align recordings even if there is non-linear
  drift of several minutes between them. A call detection and assignment pipeline can be used
  to find vocalisations and assign them to the vocalising individuals (even if the vocalisation
  is picked up on multiple microphones). The tracing and measurement functions allow for detailed
  analysis of the vocalisations and filtering of noise. Finally, the package includes a function
  to run spectrographic cross correlation, which can be used to compare vocalisations. It also
  includes multiple other functions related to analysis of vocal behaviour.
}

\details{
The main features of the package are:
  \itemize{
  \item alignment and partitioning of drifting microphones using signal compression and cross correlation
  \item call detection using an amplitude envelope
  \item fine-scale alignment and call assignment across recordings using cross correlation and energy content
  \item fundamental frequency tracing
  \item analysis of the resulting traces and wav clips
  }

The package offers functions to:
  \itemize{
  \item create flexible spectrograms
  \item run spectrographic cross correlation
  \item analyse amplitude and frequency modulation
  \item load selection tables from Raven and Audacity
  \item calculate performance of the detection and assignment functions
  }

License: GNU (>= 2)

Authors: Simeon Q. Smeele and Stephen A. Tyndel

Maintainer: Simeon Q. Smeele, <simeonqs@hotmail.com>
}
