#' Process single PCL transects.
#'
#' \code{process_pcl} imports and processes a single PCL transect.
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
#' process_pcl(data_directory, filename)
#'
#'process_pcl("./data/PCL_transects/", "oldgrowth_one.csv" )
#'

#'
#' \dontrun{
#'
#' }

process_pcl<- function(data_dir, filename, user_height){

  # This section calls the read_pcl function from the main functions
  test.data <- read_pcl(data_dir, filename)

  # main binning and process script
  bin_pcl(test.data, user_height)



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
