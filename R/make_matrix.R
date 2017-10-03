#' Make PCL matrix for higher level complexity measures
#'
#' \code{make matrix} imports and processes a single PCL transect.
#'
#' @param df data frame of unprocessed PCL data
#'
#' @keywords matrix
#' @return sorted matrix with z and x coordinates in meters
#'
#' @export
#'
#' @examples

#'
#' \dontrun{
#' make_matrix(df)
#' }



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
  message("lidar pulses")
  print(length(p$lidar.pulses))
  message("can hits")
  print(length(p$can.hits))
  message("sky hits")
  print(length(p$sky.hits) )
  replace(p, is.na(p), 0)#This will correct for any gaps w/out msmts as all NAs will be 0


}


make_matrix_part_two <- function(df) {
  #ultimately this should actually make an empty data frame or something
  p <- df

  df2 <- expand.grid(xbin = c(1:max((p$xbin))),
                     zbin = c(0:max((p$zbin))))

  #
  q <- merge(p, data.frame(table(df2[1:2])), all.y=TRUE)
  #now to add empty rows as NA
  #q <- merge(p, data.frame(table(p[1:2]))[-c(3:9)],all.y=TRUE)
  replace(q, is.na(q), 0)#This will correct for any gaps w/out mesmts as all NAs will be 0

}

# this command combines the previous functions
make_matrix <- function(df) {
  df <- make_matrix_part_one(df)
  df <- make_matrix_part_two(df)
  df$xbin <- as.integer(as.character(df$xbin))
  df$zbin <- as.integer(as.character(df$zbin))

  k <- stats::setNames(stats::aggregate(can.hits ~ xbin, data = df, FUN = max), c("xbin", "can.hits"))
  df$can.hits <- k$can.hits[match(df$xbin, k$xbin)]

  l <- stats::setNames(stats::aggregate(lidar.pulses ~ xbin, data = df, FUN = max), c("xbin", "lidar.pulses"))
  df$lidar.pulses <- l$lidar.pulses[match(df$xbin, l$xbin)]

  return(df)
}
