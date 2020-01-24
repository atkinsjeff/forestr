#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{combine_variables} combines varibles from internal functions to export
#'
#' This is a specific function combines variables together to a coherent list.
#'
#' @param variable.list a list of variables from calc_rugosity
#' @param csc.metrics a list of metrics from the csc_metrics function
#' @param rumple rumple value from calc_rumple
#' @param clumping.index clumping index value
#' @param enl effective number of layers
#' @param intensity_stats statistics on intensity values
#'
#' @keywords csc
#' @return a concatenated list of csc variables
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #This function runs interanlly and collates the slew of
#' CSC metrics and formats them.
#' #' combine_Variables(variable.list, csc.metrics, rumple, clumping.index)
#' }


combine_variables <- function(variable.list, csc.metrics, rumple, clumping.index, enl, intensity_stats){

  output.variables <- cbind(variable.list, csc.metrics, rumple, clumping.index, enl, intensity_stats)
  return(output.variables)

}
