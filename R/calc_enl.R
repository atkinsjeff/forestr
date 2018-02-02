#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{calc_enl} calculates the effective number of layers in a canopy.
#'
#'
#' @param m a data frame of VAI for x, z bins from
#'
#' @keywords enl
#' @return the effective number of layers
#' @export
#' @examples
#' # Calculates the effective number of layers
#' calc_enl(pcl_vai)
#'

calc_enl <- function(m) {
  #df = the summary matrix
  #m = vai matrix

  # Setting global variables
  total.cells  <- NULL

  # This counts the number of filled cells where there is VAI for the matrix
  df.z.count <- stats::aggregate(vai ~ zbin, data = m, FUN = function(X) sum(X != 0) )

  # The number of total filled cells (i.e. areas where there is VAI or canopy)
  total.cells <- sum(df.z.count$vai)

  # Gives the proportion of cells at each level
  df.z.count$p.i <- (df.z.count$vai / total.cells)^2

  # The effective number of layers
  enl <- 1 / (sum(df.z.count$p.i))

  message("Effective Number of Layers:  ")
  print(enl)

  return(enl)
}
