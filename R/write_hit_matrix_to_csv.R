#' Writes hit matrix to csv for further analysis
#'
#' \code{write_hit_matrix_to_csv} writes hit matrix to .csv for further analysis
#'
#' This is a specific sub-function that writes the output variables to disk in .csv format
#' and runs within the functions \code{process_pcl}, \code{process_multi_pcl}, and
#' \code{proces_tls}.
#'
#' @param m matrix of VAI with z and x coordinates
#' @param outputname name of file currently being processed
#' @param output_directory directory where output goes
#' @keywords hit matrix
#'
#' @export
#'
#' @seealso
#' \code{\link{process_pcl}}
#' \code{\link{write_pcl_to_csv}}
#' \code{\link{write_summary_matrix_to_csv}}

#'
#' @examples
#' \dontrun{
#' # This function runs internally.
#' write_hit_matrix_to_csv(m, outputname, output_directory)
#' }

write_hit_matrix_to_csv <- function(m, outputname, output_directory) {
  m.og <- m
  m <- m[, c("xbin", "zbin", "vai")]

  #Write just thie hit matrix
  filename2 <- paste(outputname, "_hit_matrix.csv", sep="")
  utils::write.csv(m, file.path(output_directory, filename2), row.names = FALSE)

  #full matrix
  filename3 <- paste(outputname, "_full_matrix.csv", sep = "")
  utils::write.csv(m.og, file.path(output_directory, filename3), row.names = FALSE)

}

