#' Intensity Statistics
#'
#' \code{calc_intensity} calcualtes statistics from the intensity column of the PCL data
#'
#' The \code{calc_intensity} function calculates statistics about the intensity
#' data in the PCL data, including min, max, sd, mean, median.
#'
#'
#' @param df data frame of uncorrected PCL data
#' @param filename name of file currently being processed
#' @keywords statisitcs
#' @export
#' @return statisics on the intensity data
#' @examples
#'
#' intensity_stats <- calc_intensity(pcl_adjusted, filename = "UVA")
#'
#####Intensity statistics

calc_intensity <- function(df, filename) {
  #Declaring global variables
  intensity <- NULL

  if (missing(filename)) {
    filename <- as.character(c(""))
  }

  z <- df
  z <- subset(z, intensity >= 0)


  message("Intensity Statistics")

  mean.intensity = mean(z$intensity, na.rm = TRUE)
  message("Mean Intensity")
  print(mean.intensity)

  median.intensity = stats::median(z$intensity, na.rm = TRUE)
  message("Median Intensity")
  print(median.intensity)

  sd.intensity = stats::sd(z$intensity, na.rm = TRUE)
  message("Standard Deviation of Intensity")
  print(sd.intensity)

  max.intensity = max(df$intensity, na.rm = TRUE)
  message("Max intensity")
  print(max.intensity)

  min.intensity = min(df$intensity, na.rm = TRUE)
  message("Minimum intensity")
  print(min.intensity)

  skew.intensity = moments::skewness(df$intensity, na.rm = TRUE)
  message("Intensity Skewness")
  print(skew.intensity)

  kurtosis.intensity = moments::kurtosis(df$intensity, na.rm = TRUE)
  message("Intensity Kurtosis")
  print(kurtosis.intensity)


  intensity.variable.list <- list(plot = filename,
                            mean.intensity = mean.intensity,
                            median.intensity = median.intensity,
                            sd.intensity = sd.intensity,
                            max.intensity = max.intensity,
                            min.intensity = min.intensity,
                            skew.intensity = skew.intensity,
                            kurtosis.intensity = kurtosis.intensity)

  intensity.variable.list <- data.frame(intensity.variable.list)
  return(intensity.variable.list)
}
