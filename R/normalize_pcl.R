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
#' normalize_pcl()
#'
#' \dontrun{
#'
#' }
normalize_pcl <- function(df) {

  df <- normalize_pcl_one(df)
  df <- normalize_pcl_two(df)
  df <- normalize_pcl_three(df)

  return(df)
}


#### light saturation correction
normalize_pcl_one <-  function(df) {
  # for loop for this jenk
  # what we are doing is counting up the number of canopy hits to an x,z point in the canopy

  # first we sort
  df <- df[with(df, order(xbin, zbin)), ]

  df$hit.count <- 0   #creates and empty column of zeros
  for (i in 1:nrow(df)) {
    x.counter = 1  #a counter! woohoo

    for(j in 2:nrow(df)){
      if(df$xbin[j] == x.counter ){

        df$hit.count[j] = df$hit.count[j-1] + df$bin.hits[j]

      }else {
        x.counter = x.counter + 1
        next
      }
      next
    }
  }
  return(df)
}

normalize_pcl_two <- function(df) {

  eq1 = (df$can.hits - df$hit.count) / df$can.hits
  eq2 = ((df$can.hits + 1) - df$hit.count) / (df$can.hits + 1)

  # if can.hits and lidar.pulses are equal, then canopy is saturated

  df <- transform(df, phi = ifelse(lidar.pulses == can.hits, eq1, eq2))
}

normalize_pcl_three <- function(df) {

  df$dee <- 0   #creates and empty column of zeros
  for (i in 1:nrow(df)) {
    x.counter = 1  #a counter! woohoo

    for(j in 2:nrow(df)){
      if(df$phi[j-1] > 0 && df$phi[j] > 0 && df$xbin[j] == x.counter ){

        df$dee[j] = log(df$phi[j-1] / df$phi[j])

      }else {
        df$dee[j] = 0
        x.counter = x.counter + 1
        next
      }
      next
    }
  }

  # now to sum up dee to make sum.dee the %total adjusted hits in the column
  q <- setNames(aggregate(dee ~ xbin, data = df, FUN = sum), c("xbin", "sum.dee"))
  df$sum.dee <- q$sum.dee[match(df$xbin, q$xbin)]




  # now to make fee a percentage of the percent hits at that level
  eq.fee = df$dee / df$sum.dee

  # for all columns where dee is >0 i.e. that is saturated
  df <- transform(df, fee = ifelse(sum.dee > 0, eq.fee, 0))
  return(df)
}
