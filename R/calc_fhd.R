#' foliar height diversity
#'
#' \code{calc_fhd} calculates foliage height diversity
#'
#' The \code{calc_fhd} function calculates foliage height diversity, where a version of the Shannon-Weiner
#' diversity index is applied to through canopy measures of gap fraction
#'
#'
#' @param m matrix of light adjusted vai values.
#' @keywords statisitcs
#' @export
#' @return foliage height diveristy
#' @examples
#'
#' calc_fhd(pcl_vai)
#'

calc_fhd <- function(m) {
  #
  fhd <- NULL
  total.vai <- NULL

  # Creates the total value of VAI for the whole transect/plot
  total.vai <- sum(m$vai)

  # A new data fram of VAI distributed at each height level
  df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

  # A new column with the proportion of the VAI in each height level to overall VAI
  df.z$ratio.vai <- df.z$vai / total.vai

  # calculates the FHD
  df.z$fhd <- df.z$ratio.vai * log(df.z$ratio.vai)

  fhd <- (sum(df.z$fhd, na.rm = TRUE) * -1)

  message("FHD!!!!!")
  print(fhd)

  return(fhd)
  }
