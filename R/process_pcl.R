#' Process single PCL transects.
#'
#' \code{process_pcl} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#'
#' @param f  the name of the filename to input
#' @param user_height the height of the laser off the ground as mounted on the user in meters
#' @param marker.spacing distance between markers, typically 10 m
#' @return writes the hit matrix, summary matrix, and output variables to csv in an output folder, along with hit grid plot
#'
#' @keywords pcl processing
#' @export
#'
#'
#' @examples
#' data_directory <- "./data/PCL_transects/"  #data directory containing PCL transects
#' filename <- "oldgrowth_one.csv"  #name of PCL transect to be processed
#' process_pcl(data_directory, filename)
#'
#'process_pcl("./data/PCL_transects/", "oldgrowth_one.csv" )
#'

#'
#' \dontrun{
#'
#' }

process_pcl<- function(f, user_height, marker.spacing){

  # Read in PCL transect
  df<- read_pcl(f)

  #cuts off the directory info to give just the filename
  filename <- sub(".*/", "", f)

  #calculate transect length
  transect.length <- get_transect_length(df, marker.spacing)

  # code hits for sky or canopy
  df <- code_hits(df)

  #adjusts by the height of the  user to account for difference in laser height to ground in meters
  df <- adjust_by_user(df, user_height)

  #not to split transects from code
  test.data.binned <- split_transects_from_pcl(df, transect.length, 10)

  csc.metrics <- csc_metrics(test.data.binned, filename)

  m1 <- make_matrix(test.data.binned)

  m2 <- normalize_pcl_one(m1)
  m3 <- normalize_pcl_two(m2)
  m4 <- normalize_pcl_three(m3)

  m5 <- calc_vai(m4)


  summary.matrix <- make_summary_matrix(test.data.binned, m5)
  rumple <- calc_rumple(summary.matrix)
  clumping.index <- calc_gap_fraction(m5)

  variable.list <- calc_rugosity(summary.matrix, m5, filename)

  output.variables <- combine_variables(variable.list, csc.metrics, rumple, clumping.index)
  print(output.variables)

  #output procedure for variables
  outputname = substr(filename,1,nchar(filename)-4)
  outputname <- paste(outputname, "output", sep = "_")
  dir.create("output", showWarnings = FALSE)
  output_directory <- "./output/"

  write_pcl_to_csv(output.variables, outputname, output_directory)
  write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
  write_hit_matrix_to_csv(m5, outputname, output_directory)

# #combining and formatting variables for output
#   combine_variables <- function(variable.list, csc.metrics, rumple, clumping.index){
#
#     output.variables <- cbind(variable.list, csc.metrics, rumple, clumping.index)
#     return(output.variables)
#
#   }


  #get filename first
  plot.filename <- tools::file_path_sans_ext(filename)

  plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

  vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  x11(width = 8, height = 6)
  hit.grid <- ggplot2::ggplot(m5, ggplot2::aes(x = xbin, y = zbin))+
    ggplot2::geom_tile(ggplot2::aes(fill = vai))+
    ggplot2::scale_fill_gradient(low="white", high="dark green",
                        limits=c(0,8.5),
                        name=vai.label)+
    #scale_y_continuous(breaks = seq(0, 20, 5))+
    # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 14),
          axis.text.y = ggplot2::element_text(size = 14),
          axis.title.x = ggplot2::element_text(size = 20),
          axis.title.y = ggplot2::element_text(size = 20))+
    ggplot2::xlim(0,transect.length)+
    ggplot2::ylim(0,41)+
    ggplot2::xlab("Distance along transect (m)")+
    ggplot2::ylab("Height above ground (m)")+
    ggplot2::ggtitle(filename)+
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

  ggplot2::ggsave(plot.file.path, hit.grid)
}
