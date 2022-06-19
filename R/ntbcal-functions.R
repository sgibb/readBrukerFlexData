#' Extracts NTBCal coefficients.
#'
#' This is a helper function to extract coefficients and constants from
#' `metaData$NtbCalStr`.
#'
#' @param x \code{character}, NTBCal line
#'
#' \preformatted{
#' "##$NTBCal= <V3.0CCalibrator 12 1 0 0 -2147483648 2147483647  V1.0CHPCData  Order 0 vCoeff V1.0VectorDouble 0  c2 0 c0 0 minMass 0 maxMass 0 bUse 0 endCHPCData V1.0CTOF2CalibrationConstants 48 0.25 3884.868239229128 1164156.9998801197 6.1682474689786355 -0.063083671657452864 -34.362308485323794 2 V1.0CTOF2CalibrationConstants 0 0 0 0 0 0 0 -1  >"
#' }
#'
#' @return a \code{double}, coefficients
#'
#' @seealso \code{\link[readBrukerFlexData]{.ctof2calibration}}
#' @noRd
#' @keywords internal
#' @return \code{double}, vector of the calibration constants.
#'
#' @keywords internal
#' @noRd
.extractV10CTOF2CalibrationConstants <- function(x) {
    x <- gsub("^.*V1.0CTOF2CalibrationConstants ([0-9. -]*) V1.0CTOF2CalibrationConstants.*$", "\\1", x)
    ## suppress warnings about NA introduction for empty strings
    suppressWarnings(as.numeric(unlist(strsplit(x, " ", fixed = TRUE))))
}

#' Cubic calibration
#'
#' Only basic support (not 100\% identical results) for Bruker Daltonics' cubic
#' calibration (encoded in `"##\$NTBCal"` as `"V1.0CTOF2CalibrationConstants"`).
#' This is an internal function and should normally not used by the user.
#'
#' @param tof \code{double}, time of flight.
#' @param d \code{double}, calibration constants, return value of
#' \code{.extractV10CTOF2CalibrationConstants}.
#' @return \code{double}, mz values.
#'
#' @note
#' Bruker Daltonics doesn't explain how the cubic calibration works.
#' All formula are results of \dQuote{trial and error}.
#' That is why mass calculated by \code{.ctof2calibration}
#' differs little from original mass.
#' See also \url{https://github.com/sgibb/readBrukerFlexData/issues/3}.
#'
#' @seealso
#' \url{https://github.com/sgibb/readBrukerFlexData/issues/3}
#'
#' @author
#' Alan Race, Samuel Granjeaud, Sebastian Gibb
#'
#' @keywords internal
#' @rdname ctof2calibration
.ctof2calibration <- function(tof, d) {
    ## e.g.  "V1.0CTOF2CalibrationConstants 48 0.25 3884.868239229128 1164156.9998801197 6.1682474689786355 -0.063083671657452864 -34.362308485323794 2 V1.0CTOF2CalibrationConstants"
    ## DELAY DW ML2 ML1 ML3 A E 2

    A <- d[6]
    B <- d[5]
    C <- sqrt(1e12 / d[4])
    D <- d[3]
    E <- d[7]

    ## quadratic: 0 = B * (sqrt(m/z))^2 + C * sqrt(m/z) + D(times)
    ## same formula as tof2mass but instead of D(times) = ML3 - tof
    ## it seems to be necessary to use D(times) = tof - ML3
    ## abs(tof - ML3) seems to be not equivalent
    ## m <- .tof2mass(tof, d[4], d[3], d[5])
    m <- (-C + sqrt(abs((C * C) - (4 * B * (tof - D))))) / (2 * B)

    s <- sign(tof - D)
    belowZero <- s < 0

    m[!belowZero] <- vapply(tof[!belowZero], function(x) {
        ## Assuming cubic with A*x^3 + B*x^2 + C*x + (D - tof)
        ## where x = sqrt(mz)
        Re(polyroot(c(D - x, C, B, A))[1L])
    }, NA_real_)

    m^2 * s - E
}
