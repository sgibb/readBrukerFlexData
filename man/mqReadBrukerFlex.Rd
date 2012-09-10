\name{mqReadBrukerFlex}
\alias{mqReadBrukerFlex}
\title{Reads mass spectrometry data into MALDIquant.}
\usage{
  mqReadBrukerFlex(path, ...)
}
\arguments{
  \item{path}{\code{character}, path to \emph{directory} or
  a single \emph{fid} file.}

  \item{\ldots}{arguments to be passed to
  \code{\link[readBrukerFlexData]{readBrukerFlexDir}} or
  \code{\link[readBrukerFlexData]{readBrukerFlexFile}}}
}
\value{
  a \code{list} of
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\description{
  This function reads all mass spectrometry data in Bruker
  Daltonics' XMASS format in a specified path into
  \code{\link[MALDIquant]{MALDIquant-package}}
  \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\details{
  See \code{\link[readBrukerFlexData]{readBrukerFlexDir}}
  or \code{\link[readBrukerFlexData]{readBrukerFlexFile}}.
}
\examples{
## load library
library("readBrukerFlexData")

## get examples directory
exampleDirectory <- system.file("Examples", package="readBrukerFlexData")

## read example spectra
spec <- mqReadBrukerFlex(file.path(exampleDirectory,
  "2010_05_19_Gibb_C8_A1"))

## plot spectra
par(mfrow=c(2, 1))
lapply(spec, plot)
par(mfrow=c(1, 1))
}
\references{
  See website:
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[readBrukerFlexData]{readBrukerFlexDir}},
  \code{\link[readBrukerFlexData]{readBrukerFlexFile}},
  \code{\link[MALDIquant]{MALDIquant-package}},
  \code{\link[MALDIquant]{MassSpectrum-class}}
}
\keyword{IO}

