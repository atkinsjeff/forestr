#' PCL diagnostic plot
#'
#' \code{pcl_diagnostic_plot} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#' @param df data frame of unprocessed PCL data
#' @param filename name of file currently being processed
#'
#' @return a plot of PCL data showing marker spacing
#'
#'
#' @examples
#'
#' \dontrun{
#' pcl_diagnostic_plot(df, filename)
#' }
pcl_diagnostic_plot <- function(df, filename) {
  #creates empty part if no filename.
  if (missing(filename)) {
    filename <- as.character(c(""))
  }

  plot(df$index,
       df$return_distance,
       main = filename,

       ylab = "Canopy Height (m)",
       xlab = "")
}
