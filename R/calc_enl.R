#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{calc_enl} imports and processes a single PCL transect.
#'
#' This is a specific function calculates canopy rugosity and other metrics
#'
#' @param m the summary matrix
#'
#' @keywords rugosity
#' @return a slew of metrics...need to fully outline later
#'
#' @export
#' @examples
#' # Calculates the effective number of layers
#' \dontrun{calc_enl()
#'}

calc_enl <- function(m) {
  #df = the summary matrix
  #m = vai matrix

  # Setting global variables
  total.cells  <-NULL

  # This counts the number of filled cells where there is VAI for the matrix
  df.z.count <- stats::aggregate(vai ~ zbin, data = m, FUN = function(X) sum(X != 0) )

  # The number of total filled cells (i.e. areas where there is VAI or canopy)
  total.cells <- sum(df.z.count$vai)

  # Gives the proportion of cells at each level
  df.z.count$p.i <- (df.z.count$vai / z)^2

  # The effective number of layers
  enl <- sum(df.z.count$p.i)

  print(enl)

  return(enl)
}
