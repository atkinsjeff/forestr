#' Writes hit matrix to csv for further analysis
#'
#' \code{write_hit_matrix_to_csv} writes hit matrix to .csv for further analysis
#'
#' This is a specific function that writes the output variables to disk in .csv format
#' @param m matrix of VAI with z and x coordinates
#' @param filename name of file currently being processed
#' @keywords hit matrix
#'
#' @export
#' @examples
#' write_hit_matrix_to_csv)
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#'
write_hit_matrix_to_csv <- function(m, filename, output_directory) {
  m <- m[, c("xbin", "zbin", "vai")]

  filename2 <- paste(filename, "_hit_matrix.csv", sep="")
  write.csv(m, file.path(output_directory, filename2))
}
