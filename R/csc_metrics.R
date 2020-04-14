#' Cover and sky fraction estimates
#'
#' \code{csc_metrics} creates first-order canopy structural metrics that
#' do not require normalization
#'
#' The \code{csc_metrics} function processes uncorrected PCL data to
#' generate canopy structural complexity (CSC) metrics that do not
#' require normalization (i.e. correction for light saturation based on
#' Beer-Lambert Law). These metrics include:  mean return height of raw data, sd
#' of raw canopy height returns, maximum measured canopy height, scan density (the
#' average no. of LiDAR returns per linear meter), and both openness and cover
#' fraction which are used for gap fraction calcuations.
#'
#'
#' @param df data frame of uncorrected PCL data
#' @param filename name of file currently being processed
#' @param transect.length the length of the transect
#' @keywords complexity
#' @export
#' @return slew of cover and sky fraction metrics
#' @examples
#'
#'
#' csc.metrics <- csc_metrics(pcl_adjusted, filename = "UVA", transect.length = 10)
#'
#####Canopy metrics before matrix creations

csc_metrics <- function(df, filename, transect.length) {
  #Declaring global variables
  return_distance <- NULL

  if (missing(filename)) {
    filename <- as.character(c(""))
  }

  z <- df
  z <- subset(z, return_distance >= 0)

  #for the cover fraction calculation, number of markers - 1
  correction.coef <- length(which((df$return_distance <= -9999)))

  message("RAW LiDAR metrics -- WARNING")
  mean.return.ht = mean(z$return_distance, na.rm = TRUE)
  message("Mean Return Height (m) of raw data")
  print(mean.return.ht)

  sd.ht = stats::sd(z$return_distance, na.rm = TRUE)
  message("Standard Deviation of raw  Canopy Height returns-- meanStd in old code")
  print(sd.ht)

  median.ht = stats::median(z$return_distance, na.rm = TRUE)
  message("Median of raw  Canopy Height returns")
  print(median.ht)

  max.ht = max(df$return_distance, na.rm = TRUE)
  message("Max Measured Canopy Height (m)")
  print(max.ht)

  scan.density = nrow(df) /transect.length
  message("Scan Density")
  print(scan.density)

  message("OPENNESS AND COVER METRICS")
  sky.fraction = (1 -  ( (length(which(df$can_hit == TRUE))) / (length(df$return_distance) - correction.coef) )) * 100
  message("Sky Fraction (%)")
  print(sky.fraction)

  cover.fraction = 100 - sky.fraction
  message("Cover Fraction (%)")
  print(cover.fraction)



  csc.variable.list <- list(plot = filename,
                            mean.return.ht = mean.return.ht,
                            sd.return.ht = sd.ht,
                            median.ht = median.ht,
                            sky.fraction = sky.fraction,
                            cover.fraction = cover.fraction,
                            max.ht = max.ht,
                            scan.density = scan.density)

  csc.variable.list <- data.frame(csc.variable.list)
  return(csc.variable.list)
}
