#' Calculates rumple
#'
#' \code{calc_rumple} calculates canopy rumple.
#'
#' This function uses the summary matrix created by
#' the function \code{make_summary_matrix} to calculate
#' canopy rumple, the relationship between outer canopy surface
#' and the ground area.
#'
#' @export
#' @param df LiDAR summary matrix data frame
#'
#' @keywords rumple
#' @return rumple for the canopy based on 2-D transect
#' @examples
#'
#' calc_rumple(pcl_summary)
#'
calc_rumple <- function(df){
  df$rump.diff <- 0

  for (i in 2:nrow(df)) {

    df$rump.diff[i] <- abs(ceiling(df$max.ht[i - 1]) - ceiling(df$max.ht[i]))

  }
  #print(df$rump.diff)
  rumple = (sum(df$rump.diff) + max(df$xbin)) / max(df$xbin)
  message("Rumple")
  print(rumple)
  return(rumple)
}
