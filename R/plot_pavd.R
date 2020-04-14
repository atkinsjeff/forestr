#' Graphs Plant Area Volume Density Profiles
#'
#' \code{plot_pavd} produces a PAVD plot from matrix data
#'
#' This function is a nested function inside of \code{process_pcl}. It could be run
#' independently using the summary_matrix.csv
#' output files created from running \code{procesS_pcl} as well.
#'
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#' @param plot.file.path.pavd path of plot file to be written, inherited
#' from \code{process_pcl} or \code{process_multi_pcl}
#' @param hist logical input to include histogram of VAI, if TRUE it is included,
#' if FALSE, it is not.
#' @param save_output if TRUE it saves the plot, if false it just runs
#' @keywords plant area volume density profile, pavd
#' @return plant area volume density plots
#'
#' @export
#'
#' @seealso
#' \code{\link{plot_hit_grid}}
#'
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' plot_pavd(pcl_vai, filename = "pcl_test", hist = FALSE, save_output = FALSE)
#' plot_pavd(pcl_vai, filename = "pcl_test", hist = TRUE, save_output = FALSE)
#'


# PAVD script
plot_pavd <- function(m, filename, plot.file.path.pavd, hist = FALSE, save_output = FALSE) {
  #m = vai matrix

pavd <- NULL

# Deal with missing stuff
if(missing(filename)){
  filename = "test"
}

if(missing(plot.file.path.pavd)){
  output_dir = 'output'
  outputname = substr(filename,1,nchar(filename)-4)
  outputname <- paste(outputname, "output", sep = "_")
  output_directory <- paste("./",output_dir,"/", sep = "")

  print(outputname)
  print(output_directory)

  plot.filename.pavd <- paste(filename, "pavd", sep = "_")

  plot.file.path.pavd <- file.path(paste(output_directory, plot.filename.pavd, ".png", sep = ""))

}
print(plot.file.path.pavd)

if(missing(hist)){
  hist = FALSE
}



# Creates the total value of VAI for the whole transect/plot
total.vai <- sum(m$vai)

# A new data fram of VAI distributed at each height level
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

# A new column with the proportion of the VAI in each height level to overall VAI
df.z$ratio.vai <- df.z$vai / total.vai

# adding an origing
origin <- data.frame(zbin = 0, vai = 0, ratio.vai = 0)

df.z <- rbind(origin, df.z)


if(hist == TRUE){
# Making PAVD plot
pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  ggplot2::geom_bar(stat = "identity", color = "light grey", alpha = 0.5)+
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text.x= ggplot2::element_text(size = 14),
        axis.text.y = ggplot2::element_text(size = 14),
        axis.title.x = ggplot2::element_text(size = 20),
        axis.title.y = ggplot2::element_text(size = 20))+
  ggplot2::coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai) + 0.05), expand = FALSE)+
  ggplot2::ylab("Plant Area Volume Density (PAVD)")+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::ggtitle(filename)+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

pavd
} else {
  pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
    ggplot2::geom_line(size = 1.5, color = "darkblue")+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x= ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20))+
    ggplot2::coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai) + 0.05), expand = FALSE)+
    ggplot2::ylab("Plant Area Volume Density (PAVD)")+
    ggplot2::xlab("Height Above Ground (m)")+
    ggplot2::ggtitle(filename)+
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

pavd
}

if(save_output == TRUE){
  output_dir = "output"
  #output procedure for variables
  dir.create(output_dir, showWarnings = FALSE)
  ggplot2::ggsave(plot.file.path.pavd, pavd, width = 6, height = 4, units = c("in"))
}
}


