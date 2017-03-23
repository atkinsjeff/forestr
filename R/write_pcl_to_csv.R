#' Writes csc metrics and output variables to .csv
#'
#' \code{write_pcl_to_csv} writes csc metrics and varialbes to .csv format
#'
#' This is a specific function that writes the output variables to disk in .csv format. It is enclosed with
#' in the process_pcl function.
#'
#' @param output.variables list of concatenated output variables
#' @param outputname name of file currently being processed
#' @param output_directory directory where output goes
#' @keywords output variables
#' @export
#' @examples
#' \dontrun{
#' write_pcl_to_csv(output_variables, outputname, output_directory)
#' }
#'
write_pcl_to_csv <- function(output.variables, outputname, output_directory) {

  filename2 <- paste(outputname, ".csv", sep="")
  write.csv(output.variables, file.path(output_directory, filename2))
}

