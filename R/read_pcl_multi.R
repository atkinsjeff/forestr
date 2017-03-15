#` Read in PCL files inside of multi read script
#'
#' \code{read_pcl_multi} imports PCL or portable canopy LiDAR files into the workspace and formats them.
#'
#' This function specificially reads in PCL files that are in .csv format, standard format for that data type.
#' @param data_directory directory where files are stored
#' @param filename name of file to be imported
#'
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' read_pcl(data_directory, )
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }


read_pcl_multi <- function(data_directory, filename) {
  f <- file.path(data_dir, filename)
  df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)
  df$index <- as.numeric(rownames(df))
  df = df[,c(3, 1, 2)]
  df
}
