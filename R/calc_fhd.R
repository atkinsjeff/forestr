#' foliar height diversity
#'
#' \code{calc_fhd} calculates foliage height diversity
#'
#' The \code{calc_fhd} function calculates foliage height diversity, where a version of the Shannon-Weiner
#' diversity index is applied to through canopy measures of gap fraction
#'
#'
#' @param m matrix of light adjusted vai values.
#' @param method "MH" is MacArthur-Horn and "Bohrer" is the Bohrer method
#'
#' @keywords statisitcs
#' @export
#' @return foliage height diveristy
#' @examples
#'
#' calc_fhd(pcl_vai, method = "Bohrer")
#'

calc_fhd <- function(m, method){

  if(method == "Bohrer"){
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
  } else if(method == "MH" ){
    fhd <- NULL
    total.lad <- NULL

    # Creates the total value of lad for the whole transect/plot
    total.lad <- sum(m$lad)

    # A new data fram of lad distributed at each height level
    df.z <- stats::aggregate(lad ~ zbin, data = m, FUN = sum)

    # A new column with the proportion of the lad in each height level to overall lad
    df.z$ratio.lad <- df.z$lad / total.lad

    # calculates the FHD
    df.z$fhd <- df.z$ratio.lad * log(df.z$ratio.lad)

    fhd <- (sum(df.z$fhd, na.rm = TRUE) * -1)

    message("FHD!!!!!")
    print(fhd)

    return(fhd)
  }
}
