#' Writes hit matrix to csv for further analysis
#'
#' \code{write_hit_matrix_to_csv} writes hit matrix to .csv for further analysis
#'
#' This is a specific function that writes the output variables to disk in .csv format
#' @param m matrix of VAI with z and x coordinates
#' @param outputname name of file currently being processed
#' @param output_directory directory where output goes
#' @keywords hit matrix
#'
#'
#' @examples
#' \dontrun{
#' # This function runs internally.
#' write_hit_matrix_to_csv(m, outputname, output_directory)
#' }

write_hit_matrix_to_csv <- function(m, outputname, output_directory) {
  m <- m[, c("xbin", "zbin", "vai")]

  filename2 <- paste(outputname, "_hit_matrix.csv", sep="")
  utils::write.csv(m, file.path(output_directory, filename2))
}
