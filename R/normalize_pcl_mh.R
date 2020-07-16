#' Normalize PCL data based on light saturation and attenuation
#'
#' \code{normalize_pcl_mh} normalizes a PCL matrix for occlusion.
#'
#' This function corrects saturated columns of LiDAR data
#' for occlusion based on assumptions from the Beer-Lambert Law.
#'
#' @param df data frame of pcl hit density processed from
#' \code{make_matrix}
#' @param k is a correction coefficent based on k = mean est. LAI/site LAI
#'
#' @keywords light
#' @return a data frame of PCL hit density corrected for
#' light saturation and attentuation based on Beer's Law
#'
#' @export
#' @examples
#' pcl_norm <- normalize_pcl_mh(pcl_matrix, k = 1)
#'
normalize_pcl_mh <-  function(df, k) {

  # what we are doing is counting up the number of canopy hits to an x,z point in the canopy
  sum.dee <- NULL
  lidar.pulses <- NULL
  can.hits <- NULL

  # If missing k default is 1 this is the coeff for the MacArthur-Horn
  if(missing(k)){
    k = 1
  }


  # first we sort
  df <- df[with(df, order(xbin, zbin)), ]

  df$pulses.in <- df$lidar.pulses #creates and empty column of zeros
  df$pulses.out <- 0
  # but we need to start out with the actual


  #split into list of data frames
  df.list <- split(df, df$xbin)

  df.list <- lapply(df.list, function(x){
    for (i in 1:nrow(x)) {
       x$pulses.out[1] <- x$lidar.pulses[1]
      return(x)
    }
  })

  df.list <- lapply(df.list, function(x){

    for (i in 1:nrow(x)) {
      x.counter = 1  #a counter! woohoo
      total.pulses = max(x$lidar.pulses)
      for(j in 2:nrow(x)){
        # if(x$xbin[j] == x.counter ){

        x$pulses.in[j] = x$pulses.in[j-1] - x$bin.hits[j-1]
        x$pulses.out[j] = x$pulses.out[j-1] - x$bin.hits[j]
      }
      return(x)
    }
  })

  df <- plyr::ldply(df.list, data.frame)
  #df <- df[-1] #drop weird columni

  # MacArthur Horn to get LAD
  df$lad <- log(df$pulses.in / df$pulses.out) * 1/k

  df$lad[df$lad == -Inf] <- 0
  df$lad[df$lad == Inf] <- 0

  # add the sum column in there
  q <- stats::setNames(stats::aggregate(lad ~ xbin, data = df, FUN = sum), c("xbin", "sum.lad"))
  df$sum.lad <- q$sum.lad[match(df$xbin, q$xbin)]

  return(df)

}
