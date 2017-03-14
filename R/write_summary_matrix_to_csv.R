#' Writes csc metrics and output variables to .csv
#'
#' \code{write_summary_matrix_to_csv} writes summary matrix to .csv format
#'
#' This is a specific function that writes the summary matrix to disk in .csv format
#'
#' @param m summary matrix
#' @param filename name of file currently being processed
#' @keywords summary matrix
#' @export
#' @examples
#' write_summary_matrix_to_csv()
#'
#' @examples
#' \dontrun{
#'
#' }
#'

write_summary_matrix_to_csv <- function(m, filename) {

  filename2 <- paste(filename, "_summary_matrix.csv", sep="")
  write.csv(m, file.path(output_directory, filename2))
}

