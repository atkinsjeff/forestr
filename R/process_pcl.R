#' Process single PCL transects.
#'
#' \code{process_pcl} imports and processes a single PCL transect.
#'
#' This is function works on either files or existing data frames in the environment. It processes raw PCL data
#' through a workflow that cuts the data into 1 meter segments with z and x positions and vertically normalizes data based on
#' light extinction assumptions from the Beer-Lambert Law to account for light saturation. Data are then
#' summarized, metrics of canopy structure complexity are calculated, and then output data saved to an output directory.
#' A hit grid plot is also saved to this same directory.
#'
#'
#' @param f  the name of the filename to input <character> or a data frame <data frame>.
#' @param user_height the height of the laser off the ground as mounted on the user in meters. default is 1 m
#' @param marker.spacing distance between markers, defaults is 10 m
#' @param max.vai the maximum value of column VAI. The default is 8. Should be a max value, not a mean.
#' @return writes the hit matrix, summary matrix, and output variables to csv in an output folder, along with hit grid plot
#'
#' @keywords pcl processing
#' @export
#'
#'
#' @examples
#'
#' # with designated file
#' process_pcl("pcl_data.csv", marker.spacing = 10, user_height = 1.05, max.vai = 8)
#'
#'
#' # with data frame
#' process_pcl(osbs, marker.spacing = 10, user_height = 1.05, max.vai = 8)
#'

process_pcl<- function(f, user_height, marker.spacing, max.vai){
  xbin <- NULL
  zbin <- NULL
  vai <- NULL

  # If missing user height default is 1 m.
  if(missing(user_height)){
    user_height = 1
  }

  # If missing marker.spacing, default is 10 m.
  if(missing(marker.spacing)){
    marker.spacing = 10
  }

  # If missing max.vai default is 8
  if(missing(max.vai)){
    max.vai = 8
  }

  if(is.character(f) == TRUE) {
  # Read in PCL transect.
  df<- read_pcl(f)

    # Cuts off the directory info to give just the filename.
  filename <- sub(".*/", "", f)

  } else if(is.data.frame(f) == TRUE){
    df <- f
    filename <- deparse(substitute(f))
  }

  # cuts out erroneous high values
  df <- df[!(df$return_distance >= 50), ]

  # Calculate transect length.
  transect.length <- get_transect_length(df, marker.spacing)

  #error checking transect length
  message("Transect Length")
  print(transect.length)

  # Desginates a LiDAR pulse as either a sky hit or a canopy hit
  df2 <- code_hits(df)

  print(table(df2$sky_hit))

  # Adjusts by the height of the  user to account for difference in laser height to ground in meters==default is 1 m.
  df3 <- adjust_by_user(df2, user_height)

  # Splits transects from code into segments (distances between markers as designated by marker.spacing
  # and chunks (1 m chunks in each marker).
  test.data.binned <- split_transects_from_pcl(df3, transect.length, marker.spacing)

  # First-order metrics of sky and cover fraction.
  csc.metrics <- csc_metrics(df3, filename, transect.length)

  # Makes matrix of z and x coordinated pcl data.
  m1 <- make_matrix(test.data.binned)

  # Normalizes date by column based on assumptions of Beer-Lambert Law of light extinction vertically
  # through the canopy.
  m2 <- normalize_pcl(m1)

  # Calculates VAI (vegetation area index m^ 2 m^ -2).
  m5 <- calc_vai(m2, max.vai)
  print(m5)
  # Summary matrix.
  summary.matrix <- make_summary_matrix(test.data.binned, m5)
  print(summary.matrix)
  rumple <- calc_rumple(summary.matrix)
  clumping.index <- calc_gap_fraction(m5)

  variable.list <- calc_rugosity(summary.matrix, m5, filename)

  output.variables <- combine_variables(variable.list, csc.metrics, rumple, clumping.index)
  #print(output.variables)

  #output procedure for variables
  outputname = substr(filename,1,nchar(filename)-4)
  outputname <- paste(outputname, "output", sep = "_")
  dir.create("output", showWarnings = FALSE)
  output_directory <- "./output/"
  print(outputname)
  print(output_directory)

  write_pcl_to_csv(output.variables, outputname, output_directory)
  write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
  write_hit_matrix_to_csv(m5, outputname, output_directory)




  #get filename first
  plot.filename <- tools::file_path_sans_ext(filename)

  plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

  vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  #x11(width = 8, height = 6)
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

  ggplot2::ggsave(plot.file.path, hit.grid, width = 8, height = 6, units = c("in"))
}
