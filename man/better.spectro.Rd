% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/better.spectro.R
\name{better.spectro}
\alias{better.spectro}
\title{better.spectro}
\usage{
better.spectro(
  wave,
  main = "",
  wl = 512,
  ovl = 450,
  xlim = NULL,
  ylim = NULL,
  mar = rep(3, 4),
  cex.main = 1,
  cex.axis = 0.75,
  cex.lab = 0.5
)
}
\arguments{
\item{wave}{wave object, e.g., from `load.wave` or `readWave`.}

\item{main}{character, title for the spectrogram. Default is no title.}

\item{wl}{numeric, window length in samples. Default is `512`.}

\item{ovl}{numeric, overlap in samples. Default is `450`.}

\item{xlim}{numeric vector of length 2, limits for the x-axis. Default is no limits.}

\item{ylim}{numeric vector of length 2, limits for the y-axis. Default is no limits.}

\item{mar}{numeric vector of length 4, the margins of the plot for the `impagep` function. Default is `rep(3, 4)`.}

\item{cex.main}{numeric the relative size of the title}

\item{cex.axis}{numeric the relative size of the axis labels.}

\item{cex.lab}{numeric the relative size of the axis titles}
}
\value{
Plots the spectrogram to current window.
}
\description{
Creates a spectrogram and plots it to the current window.
}
\examples{

require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
better.spectro(wave)

}
