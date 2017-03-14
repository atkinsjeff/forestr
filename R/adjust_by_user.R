#' Adjust by user height
#'
#' \code{adjust_by_user} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#'
#' @param df the data frame of raw pcl data
#' @param user_height the height of the laser off the ground as mounted on the user in meters
#'
#
#'
#'
#' \dontrun{
#'
#' }

adjust_by_user <- function(df, user_height) {
  df$return_distance <- df$return_distance + user_height
  df
}
