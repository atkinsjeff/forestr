#' Graphs Plant Area Volume Density Profiles
#'
#' \code{plot_pavd} produces a PAVD plot from matrix data
#'
#' This function is a nested function inside of [process_pcl]. It could be run independently using the summary_matrix.csv
#' output files created from running [procesS_pcl] as well.
#'
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#' @param plot.file.path.pavd path of plot file to be written, inherited from [process_pcl] or [process_multi_pcl]
#' @param hist logical input to include histogram of VAI, if TRUE it is included, if FALSE, it is not.
#' @keywords plant area volume density profile, pavd
#' @return plant area volume density plots
#'
#' @export
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' \dontrun{plot_pavd(summary_matrix)
#'}


# PAVD script
plot_pavd <- function(m, filename, plot.file.path.pavd, hist = FALSE) {
  #m = vai matrix

  pavd <- NULL


# Creates the total value of VAI for the whole transect/plot
total.vai <- sum(m$vai)

# A new data fram of VAI distributed at each height level
df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

# A new column with the proportion of the VAI in each height level to overall VAI
df.z$ratio.vai <- df.z$vai / total.vai

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0)

if(hist == TRUE){
# Making PAVD plot
pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
  ggplot2::geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
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

ggplot2::ggsave(plot.file.path.pavd, pavd, width = 8, height = 6, units = c("in"))
} else {
  pavd <- ggplot2::ggplot(df.z, ggplot2::aes(y = df.z$ratio.vai, x = df.z$zbin))+
    #geom_bar(stat = "identity", color = "light grey")+
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
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
}


ggplot2::ggsave(plot.file.path.pavd, pavd, width = 8, height = 6, units = c("in"))

}

