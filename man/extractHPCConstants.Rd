% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpc-functions.R
\name{.extractHPCConstants}
\alias{.extractHPCConstants}
\title{Extracts High Precision Calibration coefficients.}
\usage{
.extractHPCConstants(hpcStr)
}
\arguments{
\item{hpcStr}{metaData$hpcStr, which store coefficents
\preformatted{
hpcStr <- "V1.0CHPCData  Order 10 vCoeff V1.0VectorDouble 11
           -0.48579224953906053 0.0009361303203700988
           -6.92711401708155e-008 -1.0992953299897006e-009
           1.1718229914003113e-012 -5.392578762547374e-016
           9.0176664604755316e-020 1.9704001597871883e-023
           -1.1794161284667635e-026 2.0351573912658823e-030
           -1.2617853301428769e-034
           c2 -0.046701600316874939
           c0 237.64781433281422
           minMass 736.50266799999997
           maxMass 3698.6377320000001 bUse 1 endCHPCData"
}}
}
\value{
a \code{list}:
 \itemize{
   \item{\code{hpcConstants$coefficients}: double, vector of coefficents}
   \item{\code{hpcConstants$calibrationConstant0}: c0}
   \item{\code{hpcConstants$calibrationConstant2}: c2}
 }
}
\description{
This is a helper function to extract coefficients and constants from
metaData$hpcStr.
}
\seealso{
\code{\link[readBrukerFlexData]{.hpc}}
}
\keyword{internal}
