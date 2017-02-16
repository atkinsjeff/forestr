#' Calculates rumple
#'
#' \code{calc_rumple} imports and processes a single PCL transect.
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
#' \dontrun{
#'
#' }
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
