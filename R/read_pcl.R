#` Read in PCL files
#'
#' \code{read_pcl} imports PCL or portable canopy LiDAR files into the workspace and formats them.
#'
#' This function specificially reads in PCL files that are in .csv format, standard format for that data type.
#'
#' @param f name of file currently being processed
#' @param ht.thresh the height at which to filter values below
#'
#' @keywords read pcl raw input data
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#'
#' @seealso
#' \code{\link{process_pcl}}
#' \code{\link{process_multi_pcl}}
#'
#' @examples
#'
#' # Link to raw PCL data, in .csv form.
#' uva_pcl <- system.file("extdata", "UVAX_A4_01W.csv", package = "forestr")
#'
#' # Import PCL data to the workspace
#' pcl_data <-read_pcl(uva_pcl, ht.thresh = 60)
#'
#'

read_pcl <- function(f, ht.thresh) {
  lines_to_skip <- nullfile()

  # this script accounts for empty lines
  filedata <- readLines(f)
  lines_to_skip <- min(which(filedata != "")) - 1

  message("Number of empty lines at beginning of script")
  print(lines_to_skip)

  df <- utils::read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), skip = lines_to_skip,
                        blank.lines.skip = FALSE)[,1:2]
  df %>%
    dplyr::filter(df$return_distance < 0 | !df$return_distance %%1 == 0 | is.na(df$return_distance)) %>%
    data.frame() -> df

  # filter by height thershold
  df %>%
    dplyr::filter(df$return_distance < ht.thresh) %>%
    data.frame() -> df

  message("how many in base df have NA")
  print(sum(is.na(df$return_distance)))

  df
  df$index <- as.numeric(rownames(df))

  df = df[,c(3, 1, 2)]

  return(df)
}
