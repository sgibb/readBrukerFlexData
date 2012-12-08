## Copyright 2010-2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of readBrukerFlexData for R and related languages.
##
## readBrukerFlexData is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## readBrukerFlexData is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with readBrukerFlexData. If not, see <http://www.gnu.org/licenses/>

#' Reads mass spectrometry data into MALDIquant.
#'
#' This function is deprecated and will be removed in the next release.
#' Use \code{\link[MALDIquantForeign]{importBrukerFlex}} instead. \cr
#' This function reads all mass spectrometry data in
#' Bruker Daltonics XMASS format in a specified path into
#' \code{\link[MALDIquant]{MALDIquant-package}} 
#' \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @details
#' See \code{\link[readBrukerFlexData]{readBrukerFlexDir}} or
#' \code{\link[readBrukerFlexData]{readBrukerFlexFile}}.
#' 
#' @param path \code{character}, path to \emph{directory} or a single
#'  \emph{fid} file.
#' @param \ldots arguments to be passed to
#'  \code{\link[readBrukerFlexData]{readBrukerFlexDir}} or
#'  \code{\link[readBrukerFlexData]{readBrukerFlexFile}} 
#' 
#' @return
#'  a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @references
#' See website: \url{http://strimmerlab.org/software/maldiquant/}
#'
#' @seealso
#' \code{\link[readBrukerFlexData]{readBrukerFlexDir}},
#' \code{\link[readBrukerFlexData]{readBrukerFlexFile}},
#' \code{\link[MALDIquant]{MALDIquant-package}},
#' \code{\link[MALDIquant]{MassSpectrum-class}}
#' @keywords IO
#' @aliases mqReadBrukerFlex-deprecated
#' @rdname mqReadBrukerFlex
#' @export
mqReadBrukerFlex <- function(path, ...) {
  .Deprecated("MALDIquantForeign::importBrukerFlex")

  if (!file.exists(path)) {
    stop("Path ", sQuote(path), " doesn't exists!")
  }
  
  if (!require("MALDIquant")) {
    stop("Could not load package ", sQuote("MALDIquant"), ".")
  }
  
  if (!file.info(path)$isdir) {
    s <- readBrukerFlexFile(fidFile=path, ...)
    return(list(createMassSpectrum(mass=s$spectrum$mass, 
                                   intensity=s$spectrum$intensity, 
                                    metaData=s$metaData)))
  } else {
    s <- readBrukerFlexDir(brukerFlexDir=path, ...)
    s <- lapply(s, function(x) {
      return(createMassSpectrum(mass=x$spectrum$mass, 
                                intensity=x$spectrum$intensity, 
                                metaData=x$metaData))
    })
    return(s)
  }
}
 
