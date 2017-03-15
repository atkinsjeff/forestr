#' Writes csc metrics and output variables to .csv
#'
#' \code{write_pcl_to_csv} writes csc metrics and varialbes to .csv format
#'
#' This is a specific function that writes the output variables to disk in .csv format
#' @param output.variables list of concatenated output variables
#' @param filename name of file currently being processed
#'
#' @keywords output variables
#' @export
#' @examples
#' write.pcl.to.csv()
#'
#' @examples
#' \dontrun{
#'
#' }
#'
write_pcl_to_csv <- function(output.variables, filename, output_directory) {



  filename2 <- paste(filename, ".csv", sep="")
  write.csv(output.variables, file.path(output_directory, filename2))
}

