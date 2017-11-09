#' Normalize PCL data based on light saturation and attenuation
#'
#' \code{normalize_pcl} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#' @param df data frame of pcl hit density
#'
#' @keywords light
#' @return a data frame of PCL hit density corrected for light saturation and attentuation based on Beer's Law
#'
#' @export
#' @examples
#'
#' \dontrun{
#' normalize_pcl(df)
#' }
normalize_pcl <- function(df) {

  sum.dee <- NULL
  lidar.pulse <- NULL
  can.hits <- NULL


  df <- normalize_pcl_one(df)
  df <- normalize_pcl_two(df)
  df <- normalize_pcl_three(df)

  return(df)
}



#### light saturation correction
normalize_pcl_one <-  function(df) {
  # for loop for this jenk
  # what we are doing is counting up the number of canopy hits to an x,z point in the canopy
  sum.dee <- NULL
  lidar.pulses <- NULL
  can.hits <- NULL

  # first we sort
  df <- df[with(df, order(xbin, zbin)), ]

  df$hit.count <- 0   #creates and empty column of zeros

  #split into list of data frames
  df.list <- split(df, df$xbin)

  df.list <- lapply(df.list, function(x){

    for (i in 1:nrow(x)) {
      x.counter = 1  #a counter! woohoo

      for(j in 2:nrow(x)){
        # if(x$xbin[j] == x.counter ){

          x$hit.count[j] = x$hit.count[j-1] + x$bin.hits[j]
      }
    return(x)
    }
      })

  df <- plyr::ldply(df.list, data.frame)
  df <- df[-1] #drop weird columni
  return(df)

}


normalize_pcl_two <- function(df) {
  sum.dee <- NULL
  lidar.pulses <- NULL
  can.hits <- NULL

  #saturated
  eq1 = ((df$can.hits + 1) - df$hit.count) / (df$can.hits + 1)
  #unsaturated
  eq2 = (df$can.hits - df$hit.count) / df$can.hits

  # if can.hits and lidar.pulses are equal, then canopy is saturated
  df <- transform(df, phi = ifelse(lidar.pulses == can.hits, eq1, eq2))

  df <- replace(df, is.na(df), 0)
  #df[is.na(df)] <- 0


}

normalize_pcl_three <- function(df) {
  sum.dee <- NULL
  df$dee <- 0   #creates and empty column of zeros
  #split into list of data frames
  df.list <- split(df, df$xbin)

  df.list <- lapply(df.list, function(x){


    for(i in 1:nrow(x)) {
      x.counter = 1 # a counter
      for(j in 2:nrow(x)){
        x.counter = 1 # a counter

        if(x$phi[j-1] > 0 && x$phi[j] > 0){

          x$dee[j] = log(x$phi[j-1] / x$phi[j])
          x$x.counter = x.counter
        }else {
          x$dee[j] = 0
          x$x.counter = x.counter
        }

      }
      x.counter = x.counter + 1
      return(x)
    }
  })
#
  df2 <- plyr::ldply(df.list, data.frame)
#
#   # # now to sum up dee to make sum.dee the %total adjusted hits in the column
   q <- stats::setNames(stats::aggregate(dee ~ xbin, data = df2, FUN = sum), c("xbin", "sum.dee"))
   df2$sum.dee <- q$sum.dee[match(df2$xbin, q$xbin)]
#   # #
#   # #
#   # #
#   # #
#   # # # now to make fee a percentage of the percent hits at that level
    eq.fee = df2$dee / df2$sum.dee
#   # #
#   # # # for all columns where dee is > 0 i.e. that is saturated
    df2 <- transform(df2, fee = ifelse(sum.dee > 0, eq.fee, 0))
    return(df2)
}

