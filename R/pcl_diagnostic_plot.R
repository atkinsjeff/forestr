#' PCL diagnostic plot
#'
#' \code{pcl_diagnostic_plot} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' }
pcl_diagnostic_plot <- function(df, filename) {
  plot(df$index,
       df$return_distance,
       main = filename,

       ylab = "Canopy Height (m)",
       xlab = "")
}
