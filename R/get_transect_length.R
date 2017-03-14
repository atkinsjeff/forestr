#' Get transect length of PCL transect (in meters)
#'
#' \code{get_transect_length} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#' @param df data frame of unprocessed PCL data
#' @param marker.spacing distance between transect markers, typically 10 m
#' @return length of transect
#'
#' @examples
#'
#' \dontrun{
#'
#' }

get_transect_length <- function (df, marker.spacing) {

  transect.length <- (length(which((df$return_distance <= -9999))) - 1) * marker.spacing
  return(transect.length)

}
