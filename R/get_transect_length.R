#' Get transect length of PCL transect (in meters)
#'
#' \code{get_transect_length} acquires the length of a transect based on
#' a known marker spacing of the data markers stored in pcl data.
#'
#' Returns the transect length of a given PCL file given a known marker spacing.
#'
#' @param df data frame of unprocessed PCL data
#' @param marker.spacing distance between transect markers, typically 5 or 10 m
#' @return length of transect
#' @export
#' @examples
#'
#' # Get the length of the transect given a known spacing between data markers
#' transect.length <- get_transect_length(pcl_data, marker.spacing)
#'

get_transect_length <- function (df, marker.spacing) {

  transect.length <- (length(which((df$return_distance <= -9999))) - 1) * marker.spacing
  return(transect.length)

}
