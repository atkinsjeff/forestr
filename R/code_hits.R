#' Code hits
#'
#' \code{code_hits} classifies data values as canopy returns, sky returns, or
#' data markers.
#'
#'  The function \code{code_hits} accounts for the NAs that are in
#'  the return distance column which are actually
#'   the sky hits (i.e. when the lidar does not record a canopy hit).
#
#' @param df a raw set of pcl data
#' @export
#'
#' @examples
#' # classify data values that have been imported using read_pcl
#' pcl_coded <- code_hits(pcl_data)
#'
code_hits <- function(df) {
  # Start with all false
  df$sky_hit = FALSE
  df$can_hit = FALSE
  df$marker = FALSE

  # NA return indicates sky hit
  df$sky_hit[is.na(df$return_distance)] = TRUE

  # positive return indicates canopy hit
  df$can_hit[(df$return_distance > 0) & !df$sky_hit] = TRUE

  # If not sky or canopy, it's a marker hit
  df$marker[!df$sky_hit & !df$can_hit] = TRUE

  return(df)
}
