#' Make PCL matrix for higher level complexity measures
#'
#' \code{make matrix} imports and processes a single PCL transect.
#'

#'
#'
#' @examples
#' data_directory <- "./data/PCL_transects/"  #data directory containing PCL transects
#' filename <- "oldgrowth_one.csv"  #name of PCL transect to be processed
#' process_pcl(data_directory, filename)
#'
#'process_pcl("./data/PCL_transects/", "oldgrowth_one.csv" )
#'
#' \dontrun{
#'
#' }



make_matrix_part_one <- function(df) {
  #ultimately this should actually make an empty data frame or something
  #and it should go from x 1:40 and z to whatever so there are empty values in there
  z = df
  #z <- subset(z, return_distance >= 0)
  # zz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = mean), c("xbin", "mean.ht"))
  # zzz <-setNames(aggregate(return_distance ~ xbin, data = z, FUN = sd), c("xbin", "sd.ht"))
  # zzzz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = max), c("xbin", "max.ht"))

  # # number of lidar returns for entire column
  # l <- setNames(aggregate(index ~ xbin, data = df, FUN = length), c("xbin", "lidar.pulses"))
  #print(l)
  # number of return per x,z bin in the canopy
  m <- setNames(aggregate(return_distance ~ xbin + zbin, data = df, FUN = length), c("xbin", "zbin", "bin.hits"))
  m <- m[!m$zbin < 0, ]

  # number of sky.hits per column (x)
  n <- setNames(aggregate(sky_hit ~ xbin, data = df, FUN = sum), c("xbin", "sky.hits"))

  # number of canopy returns in column
  k <- setNames(aggregate(can_hit ~ xbin, data = df, FUN = sum), c("xbin", "can.hits"))


  #print(k)
  #p <- Reduce(function(x, y) merge(x,y, all = TRUE), list(m, l, n, k))
  p <- merge(m, n, by = c("xbin"), all = TRUE)
  p <- merge(p, k, by = c("xbin"), all = TRUE)

  p$lidar.pulses <- p$can.hits + p$sky.hits
  # p <- merge(p, zz, by = c("xbin"), all = TRUE)
  # p <- merge(p, zzz, by = c("xbin"), all = TRUE)
  # p <- merge(p, zzzz, by = c("xbin"), all = TRUE)
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

  k <- setNames(aggregate(can.hits ~ xbin, data = df, FUN = max), c("xbin", "can.hits"))
  df$can.hits <- k$can.hits[match(df$xbin, k$xbin)]

  l <- setNames(aggregate(lidar.pulses ~ xbin, data = df, FUN = max), c("xbin", "lidar.pulses"))
  df$lidar.pulses <- l$lidar.pulses[match(df$xbin, l$xbin)]

  return(df)
}
