#' Process single PCL transects.
#'
#' \code{process_pcl_transect} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory
#' and a filename in that directory.Future versions of this will allow for direct
#' input of file into command so there will be no need to specify both directory and file.
#'
#'
#'
#'
#' @examples
#' data_directory <- "./data/PCL_transects/"  #data directory containing PCL transects
#' filename <- "oldgrowth_one.csv"  #name of PCL transect to be processed
#' process_pcl_transect(data_dir, filename)
#'
#'process_pcl_transect("./data/PCL_transects/", "oldgrowth_one.csv" )
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#'
#' }

process_pcl_transect <- function(data_dir, filename){

  # This section calls the read_pcl function from the main functions
  test.data <- read.pcl(data_dir, filename)
  transect.length <- get.transect.length(test.data)
  test.2 <- code_hits(test.data)


  #adjusts by the height of the  user to account for difference in laser height to ground
  test.2 <- adjust_by_user(test.2, 1.2)

  # need to code in diagnostic plot better
  #pcl.diagnostic.plot(test.2, filename)

  test.data.binned <- split_transects_from_pcl(test.2, transect.length, 10)

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

  outputname = substr(filename,1,nchar(filename)-4)
  outputname <- paste(outputname, "output", sep = "_")
  dir.create("output", showWarnings = FALSE)
  output_directory <- "./output/"

  write.pcl.to.csv(output.variables, outputname)
  write.summary.matrix.to.csv(summary.matrix, outputname)
  write.hit.matrix.to.csv(m5, outputname)




  #get filename first
  plot.filename <- file_path_sans_ext(filename)

  plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

  vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  x11(width = 8, height = 6)
  hit.grid <- ggplot(m5, aes(x = xbin, y = zbin))+
    geom_tile(aes(fill = vai))+
    scale_fill_gradient(low="white", high="dark green",
                        limits=c(0,8.5),
                        name=vai.label)+
    #scale_y_continuous(breaks = seq(0, 20, 5))+
    # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x= element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20))+
    xlim(0,transect.length)+
    ylim(0,41)+
    xlab("Distance along transect (m)")+
    ylab("Height above ground (m)")+
    ggtitle(filename)+
    theme(plot.title = element_text(lineheight=.8, face="bold"))

  ggsave(plot.file.path, hit.grid)
}
