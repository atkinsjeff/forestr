#` Read in PCL files inside of multi read script
#'
#' \code{read_pcl_multi} imports PCL or portable canopy LiDAR files into the workspace and formats them.
#'
#' This function specificially reads in PCL files that are in .csv format, standard format for that data type.
#' @param data_directory directory where files are stored
#' @param filename name of file to be imported
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples

#' \dontrun{
#' # This function runs internally right now.
#' read_pcl_multi(data_directory, filename)
#' }


read_pcl_multi <- function(data_directory, filename) {
  f <- file.path(data_dir, filename)
  df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)
  df$index <- as.numeric(rownames(df))
  df = df[,c(3, 1, 2)]
  df
}
