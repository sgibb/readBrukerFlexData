\name{.readAcquFile}
\alias{.readAcquFile}
\title{Reads an acqu file.}
\usage{
  .readAcquFile(fidFile, verbose = FALSE)
}
\arguments{
  \item{file}{\code{character}, path to corresponding fid
  file (e.g.
  \code{Pankreas_HB_L_061019_A10/0_a19/1/1SLin/fid})}

  \item{verbose}{\code{logical}, print verbose messages?}
}
\value{
  a \code{list} with metadata
}
\description{
  This function reads constants, calibrations values and a
  lot of more from \emph{acqu} files.
}
\seealso{
  \code{\link[readBrukerFlexData]{readBrukerFlexFile}},
}
\keyword{internal}
\keyword{IO}

