#' Code hits
#'
#' \code{code_hits} imports and processes a single PCL transect.
#'
#'  this function accounts for the NAs that are in return distance which are actually
#'   the sky hits (i.e. when the lidar does not record a canopy hit)
#
#' @param df a raw set of pcl data
#' @export
#'
#' @examples
#' \dontrun{
#' code_hits(df)
#' }
code_hits <- function(df) {
  for(i in 1:nrow(df)){
    if (is.na(df$return_distance[i]) == TRUE) {
      df$sky_hit[i] = TRUE
      df$can_hit[i] = FALSE
      df$marker[i] = FALSE
    }else{
      if (df$return_distance[i] > 0){
        df$sky_hit[i] = FALSE
        df$can_hit[i] = TRUE
        df$marker[i] = FALSE
      }else{
        df$sky_hit[i] = FALSE
        df$can_hit[i] = FALSE
        df$marker[i] = TRUE

      }
    }
  }
  return(df)
}
