#` Read in PCL files
#'
#' \code{read_pcl} imports PCL or portable canopy LiDAR files into the workspace and formats them.
#'
#' This function specificially reads in PCL files that are in .csv format, standard format for that data type.
#'
#' @param f name of file currently being processed
#'
#' @keywords read pcl raw input data
#' @export
#'
#' @examples
#' \dontrun{
#' read_pcl(f)
#'  }
#'

read_pcl <- function(f) {
  df <- utils::read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)[,1:2]
  df$index <- as.numeric(rownames(df))
  df = df[,c(3, 1, 2)]
  return(df)
}
