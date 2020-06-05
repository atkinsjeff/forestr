#` Read in PCL files inside of multi read script
#'
#' \code{read_pcl_multi} imports PCL or portable canopy LiDAR files into the workspace and formats them.
#'
#'
#' This function specificially reads in PCL files that are in .csv format, standard format for that data type.
#' @param data_directory directory where files are stored
#' @param filename name of file to be imported
#' @param ht.thresh the height at which to filter values below
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#'
#' @examples
#' \dontrun{
#' # This function runs internally right now.
#' read_pcl_multi(data_directory, filename, ht.thresh)
#' }


read_pcl_multi <- function(data_directory, filename, ht.thresh) {
  f <- file.path(data_directory, filename)

  lines_to_skip <- nullfile()

  # this script accounts for empty lines
  filedata <- readLines(f)
  lines_to_skip <- min(which(filedata != "")) - 1
  # cat(i, files[i], lines_to_skip, "\n")

  print(filename)
  message("Number of empty lines at beginning of script")
  print(lines_to_skip)

  df <- utils::read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), skip = lines_to_skip, blank.lines.skip = FALSE)[,1:2]

  df %>%
    dplyr::filter(df$return_distance < 0 | !df$return_distance %%1 == 0 | is.na(df$return_distance)) %>%
    data.frame() -> df

  # filter by height thershold
  df %>%
    dplyr::filter(df$return_distance < ht.thresh) %>%
    data.frame() -> df

  df$index <- as.numeric(rownames(df))
  df = df[,c(3, 1, 2)]
  df
}
