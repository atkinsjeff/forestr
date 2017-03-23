#' Cover and sky fraction estimates
#'
#' \code{csc_metrics} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename or a single data frame in that directory.
#'
#' @param df data frame of uncorrected PCL data
#' @param filename name of file currently being processed
#'
#' @keywords csc, structure, rugosity, sky fraction, canopy fraction
#' @export
#' @return slew of cover and sky fraction metrics
#' @examples
#'
#' \dontrun{
#' csc_metrics(df, filename)
#' }
#####Canopy metrics before matrix creations

csc_metrics <- function(df, filename) {
  #Declaring global variables
  return_distance <- NULL

  if (missing(filename)) {
    filename <- as.character(c(""))
  }

  z <- df
  z <- subset(z, return_distance >= 0)

  #for the cover fraction calculation, number of markers - 1
  correction.coef <- length(which((df$return_distance <= -9999)))

  mean.return.ht = mean(z$return_distance, na.rm = TRUE)
  message("Mean Return Height (m) of raw data -- meanHeight in old code")
  print(mean.return.ht)

  sd.ht = stats::sd(z$return_distance, na.rm = TRUE)
  message("Standard Deviation of raw  Canopy Height returns-- meanStd in old code")
  print(sd.ht)

  sky.fraction = (1 - (length(which(df$can_hit == TRUE)) / (length(df$return_distance) - correction.coef) )) * 100
  message("Sky Fraction (%)")
  print(sky.fraction)

  cover.fraction = 100 - sky.fraction
  message("Cover Fraction (%)")
  print(cover.fraction)

  max.ht = max(df$return_distance, na.rm = TRUE)
  message("Max Measured Canopy Height (%)")
  print(max.ht)

  csc.variable.list <- list(plot = filename,
                            mean.return.ht = mean.return.ht,
                            sd.return.ht = sd.ht,
                            sky.fraction = sky.fraction,
                            cover.fraction = cover.fraction,
                            max.ht = max.ht)
  csc.variable.list <- data.frame(csc.variable.list)
  return(csc.variable.list)
}
