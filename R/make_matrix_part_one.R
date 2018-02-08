#' Make PCL matrix part one
#'
#' \code{make_matrix_part_one} produces a matrix of, x, z values in
#' coordinate space with the number and type of each LiDAR
#' return in each x, z bin combination
#'
#'
#' @param df data frame of PCL data that has been processed with
#'
#' @keywords matrix
#' @return sorted matrix of LiDAR returns for each x, z position
#'
#'
#'
#'
make_matrix_part_one <- function(df) {
  #ultimately this should actually make an empty data frame or something
  #and it should go from x 1:40 and z to whatever so there are empty values in there
  z = df
  # number of return per x,z bin in the canopy
  m <- stats::setNames(stats::aggregate(return_distance ~ xbin + zbin, data = df, FUN = length), c("xbin", "zbin", "bin.hits"))
  m <- m[!m$zbin < 0, ]

  # number of sky.hits per column (x)
  n <- stats::setNames(stats::aggregate(sky_hit ~ xbin, data = df, FUN = sum), c("xbin", "sky.hits"))

  # number of canopy returns in column
  k <- stats::setNames(stats::aggregate(can_hit ~ xbin, data = df, FUN = sum), c("xbin", "can.hits"))

  p <- merge(m, n, by = c("xbin"), all = TRUE)
  p <- merge(p, k, by = c("xbin"), all = TRUE)

  p$lidar.pulses <- p$can.hits + p$sky.hits

  replace(p, is.na(p), 0)#This will correct for any gaps w/out msmts as all NAs will be 0


}

