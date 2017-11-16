#' Process single PCL transects.
#'
#' \code{process_tls} imports and processes a slice from a voxelated TLS scan.
#'
#' This function takes as input a four column .CSV file or data frame of x, y, z, and VAI
#' (Vegetation Area Index) derived from 3-D LiDAR data. Currently, this function only analyzes
#' a single slice from the inputed TLS data set. VAI is calculated externally by the user using
#' user-determined methodologyy.
#'
#'
#' @param f  the name of the filename to input <character> or a data frame <data frame>.
#' @param pavd logical input to include Plant Area Volume Density Plot from [plot_pavd], if TRUE it is included, if FALSE, it is not.
#' @param hist logical input to include histogram of VAI with PAVD plot, if TRUE it is included, if FALSE, it is not.
#' @param slice the number of the transect to use from xyz tls data
#'
#' @return writes the hit matrix, summary matrix, and output variables to csv in an output folder, along with hit grid plot
#'
#' @keywords tls processing
#' @export
#'
#'
#' @examples
#'
#' # with designated file
#' \dontrun{process_tls("tls.csv", slice = 5, pavd = FALSE, hist = FALSE)
#' }
#'

process_tls<- function(f, slice, pavd = FALSE, hist = FALSE){
  xbin <- NULL
  zbin <- NULL
  vai <- NULL
  x <- NULL


  if(is.character(f) == TRUE) {

    # Read in PCL transect.
    df.xyz <- utils::read.csv(f, header = FALSE, col.names = c("x", "y", "z", "vai"), blank.lines.skip = FALSE)

  # Cuts off the directory info to give just the filename.
    filename <- sub(".*/", "", f)

  } else if(is.data.frame(f) == TRUE){
    df.xyz <- f
    filename <- deparse(substitute(f))
  } else {
    warning('This is not the data you are looking for')
  }

  # Data munging TLS df to PCL style slice

  # this selects the east most transect
  df <- dplyr::filter(df.xyz, x == slice)

  # renaming columns. The y value gets renamed xbin because it is now a transect
  m1 <- plyr::rename(df, c( "y" = "xbin", "z" = "zbin", "vai" = "vai"))

  # derive tls mean leaf height
  m2 <- calc_tls_mean_leaf_ht(m1)


  variable.list <- calc_tls_csc(m2, filename)

  #
  # # adjusting to pcl format
  # m.tls$xbin <- m.tls$xbin + 21
#
#   # Summary matrix.
#   summary.matrix <- make_summary_matrix(test.data.binned, m5)
#   rumple <- calc_rumple(summary.matrix)
#   clumping.index <- calc_gap_fraction(m5)
#
#
#   # effective number of layers
#   enl <- calc_enl(m5)

  output.variables  <- cbind(variable.list)

  #output procedure for variables
  outputname = substr(filename,1,nchar(filename)-4)
  outputname <- paste(outputname, "output", sep = "_")
  dir.create("output", showWarnings = FALSE)
  output_directory <- "./output/"
  print(outputname)
  print(output_directory)

  write_pcl_to_csv(output.variables, outputname, output_directory)
  # write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
  # write_hit_matrix_to_csv(m5, outputname, output_directory)

  transect.length <- max(m2$xbin)



  #get filename first
  plot.filename <- tools::file_path_sans_ext(filename)
  plot.filename.full <- paste(plot.filename, "hit_grid", sep = "_")
  plot.filename.pavd <- paste(plot.filename, "pavd", sep = "_")

  plot.file.path.hg <- file.path(paste(output_directory, plot.filename.full, ".png", sep = ""))
  plot.file.path.pavd <- file.path(paste(output_directory, plot.filename.pavd, ".png", sep = ""))

  vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  #x11(width = 8, height = 6)
  hit.grid <- ggplot2::ggplot(m2, ggplot2::aes(x = xbin, y = zbin))+
    ggplot2::geom_tile(ggplot2::aes(fill = vai))+
    ggplot2::scale_fill_gradient(low="gray88", high="dark green",
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

  ggplot2::ggsave(plot.file.path.hg, hit.grid, width = 8, height = 6, units = c("in"))

  # PAVD
  if(pavd == TRUE && hist == FALSE){

    plot_pavd(m2, filename, plot.file.path.pavd)
  }
  if(pavd == TRUE && hist == TRUE){

    plot_pavd(m2, filename, plot.file.path.pavd, hist = TRUE)
  }

}
