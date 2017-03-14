#' Process multiplie PCL transects.
#'
#' \code{process_multi_pcl} imports and processes a single PCL transect.
#'
#' This is a specific function that works using the input of a data directory of .csv
#' files where the function cycles through the files there.
#'
#' @param data_dir directory where PCL .csv files are stored
#' @param user_height height of laser from ground based on user in meters
#' @param marker.spacing space between markers in the PCL data, in meters
#'
#'
#' @examples
#' data_directory <- "./data/PCL_transects/"  #data directory containing PCL transects
#' process_multi_pcl(data_directory)
#'
#'process_multi_pcl("./data/PCL_transects/")
#'

#'
#' \dontrun{
#'
#' }
process_multi_pcl <- function(data_dir, user_height, marker.spacing){



  file.names <- dir(data_dir, pattern =".CSV")
  length(file.names)

  for(i in 1:length(file.names)){
    filename <- file.names[i]


    process_pcl(data_dir, filename, user_height, marker.spacing)



    #get filename first
    plot.filename <- file_path_sans_ext(filename)

    plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

    vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
    #x11(width = 8, height = 6)
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


    write.pcl.to.csv <- function(output.variables, filename) {

      filename2 <- paste(filename, ".csv", sep="")
      write.csv(output.variables,file.path(output_directory, filename2))
    }

    write.summary.matrix.to.csv <- function(m, filename) {

      filename2 <- paste(filename, "_summary_matrix.csv", sep="")
      write.csv(m, file.path(output_directory, filename2))
    }

    write.hit.matrix.to.csv <- function(m, filename) {
      m <- m[, c("xbin", "zbin", "vai")]

      filename2 <- paste(filename, "_hit_matrix.csv", sep="")
      write.csv(m, file.path(output_directory, filename2))
    }
  }
}
