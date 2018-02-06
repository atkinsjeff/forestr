#' PCL diagnostic plot
#'
#' \code{pcl_diagnostic_plot} this function provides a diagnostic view of raw PCL data
#'
#' This function provides a graphic view of raw PCL data to check for equal data
#' spacing and marker spacing
#'
#' @param df data frame of unprocessed PCL data
#' @param filename name of file currently being processed
#'
#' @return a plot of PCL data showing marker spacing
#'
#'
#' @examples
#'# using the Ordway-Swisher Data set
#' pcl_diagnostic_plot(osbs)
#'
pcl_diagnostic_plot <- function(df, filename) {
  #creates empty part if no filename.
  if (missing(filename)) {
    filename <- as.character(c(""))
  }

  graphics::plot(df$index,
       df$return_distance,
       main = filename,

       ylab = "Canopy Height (m)",
       xlab = "")
}
