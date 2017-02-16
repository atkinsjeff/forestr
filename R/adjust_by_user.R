#' Adjust by user height
#'
#' \code{adjust_by_user} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#'
#'
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' }
adjust_by_user <- function(df, user.ht) {
  df$return_distance <- df$return_distance + user.ht
  df
}
