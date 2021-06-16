#' Calculate gap fraction
#'
#' \code{calc_gap_fraction} produces clumping index based on
#' gap fraction through the canopy.
#'
#' This is a specific function that works using the adjusted matrix to calculate gap fraction through the canopy. This function also returns clumping index.
#'
#' @param m the matrix of bin hits calculated as density of
#' LiDAR returns for each x column.
#'
#' @keywords gap fraction
#' @export
#'
#' @examples
#' calc_gap_fraction(pcl_vai)
#'


calc_gap_fraction <- function(m){
  transect.length <- max(m$xbin)

  for(i in 1:nrow(m)){
    if (m$bin.hits[i] == 0) {
      m$gap[i] = 1
    }else{
      m$gap[i] = 0
    }
  }
  # the thinking here is that you can average across the z plane to get the gap fraction, or the portion of sky/canopy unobstructed by canopy. so if four pixels out of five were empty, that would be 0.8 gap fraction  or the mean(c(1,1,1,1,0))--think of the 1 as gap = true
  #print(m$gap)
  gap.list <- stats::setNames(stats::aggregate(gap ~ zbin, data = m, FUN = mean, na.rm = FALSE, na.action = 'na.pass'), c("zbin", "gap.fraction"))
  #print(gap.list)

  mean.gap.fraction = mean(gap.list$gap.fraction)
  message("Mean Gap Fraction ---as error check should be same as porosity")
  print(mean.gap.fraction)

  message("now we replace the 0's with 1's so when we take the ln they = 0")
  gap.list[gap.list == 0] <- 1

  #print(gap.list)
  gap.list$ln.gap.fraction <- log(gap.list$gap.fraction)

  #print(gap.list)
  clump <- log(mean.gap.fraction) / mean(gap.list$ln.gap.fraction)
  message("Clumping Index")
  print(clump)
  return(clump)

  gap.variables <- list(ci = clump,
                        gap_fraction = mean.gap.fraction)

  return(gap.variables)

}
