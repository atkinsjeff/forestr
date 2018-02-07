#' Adjust by user height
#'
#' \code{adjust_by_user} adjusts data based on the user height to acccount
#' for the laser's distance from the ground.
#'
#' The function \code{adjust_by_user} simply adds the height of the user to the
#' return distances in the data frame to estimate true height.
#'
#'
#' @param df the data frame of raw pcl data
#' @param user_height the height of the laser off the ground as mounted on the
#' user in meters
#' @return a data frame adjusted by height
#' @export
#'
#' @examples
#' # Adust raw data to account for user height as PCL is user-mounted and correction
#' # gives actual distance from ground.
#'
#' pcl_adjusted <- adjust_by_user(pcl_coded, user_height = 1.05)
#'

adjust_by_user <- function(df, user_height) {
  if(missing(user_height)){
    user_height = 1
  }
  df$return_distance <- df$return_distance + user_height
  df
}
