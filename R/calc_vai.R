#' Calculate vegetation area index (VAI) from normalized PCL data matrix
#'
#' \code{calc_vai} imports and processes a single PCL transect.
#'
#' @param df data frame of pcl data that has been corrected for light extinction
#' @param max.vai the maximum value of column VAI. The default is 8. Should be a max value, not a mean.
#' @keywords vai
#' @return a matrix of vai by x, z in the canopy
#' @export
#'
#' @examples
#' \dontrun{
#' calc_vai(df)
#' }
#####this series of functions creates VAI
calc_vai <- function(df, max.vai) {
  #declaring global variables
  lidar.pulses <- NULL
  can.hits <- NULL
  fee <- NULL


  #calculates the coefficent for the eq.olai step based on max VAI/LAI
  vai.coeff <- 1 - (exp(-(max.vai/2)))

  # this should be how much cover (cvr) is in each, x,z bin index value
  df$cvr <- (df$bin.hits / df$can.hits)

  # olai has a maximum value of 8. eq one is for use on areas that are not saturated
  eq.olai = (log(1.0 - (df$can.hits/df$lidar.pulses) * vai.coeff)  * -1) /0.5

  ##### you need to do this for all of those columns! olai by column
  # for all columns that are less than saturated, adjust olai
  df <- transform(df, olai = ifelse(lidar.pulses > can.hits, eq.olai, max.vai))

  # now make adjusted vai
  eq.vai1 = df$olai * df$fee
  eq.vai2 = df$olai * df$cvr
  df <- transform(df, vai = ifelse(lidar.pulses == can.hits, eq.vai1, eq.vai2))

  # df[is.na(df)] <- 0


  # df$vai <- (log(1.0 - df$cvr*0.9817)  * -1) /0.5
  df[is.na(df)] <- 0
  return(df)

}
