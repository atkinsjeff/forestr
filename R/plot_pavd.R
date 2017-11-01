#' Graphs Plant Area Volume Density Profiles
#'
#' \code{plot_pavd} produces a PAVD plot from matrix data
#'
#' This is a specific function calculates canopy rugosity and other metrics
#'
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#' @param plot.file.path path of plot file to be written, inherited from [process_pcl] or [process_multi_pcl]
#' @keywords plant area volume density profile, pavd
#' @return plant area volume density plots
#'
#' @export
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' \dontrun{plot_pavd()
#'}


# PAVD script
plot_pavd <- function(m, filename, plot.file.path.pavd, hist = FALSE) {
  #m = vai matrix


# Creates the total value of VAI for the whole transect/plot
total.vai <- sum(m$vai)

# A new data fram of VAI distributed at each height level
df.z <- aggregate(vai ~ zbin, data = m, FUN = sum)

# A new column with the proportion of the VAI in each height level to overall VAI
df.z$ratio.vai <- df.z$vai / total.vai

# Shape factor for spline
sf <- sum(df.z$ratio.vai > 0)

if(hist == TRUE){
# Making PAVD plot
pavd <- ggplot2::ggplot(df.z, aes(y = df.z$ratio.vai, x = df.z$zbin))+
  geom_bar(stat = "identity", color = "light grey")+
  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
  ggplot2::theme_classic()+
  ggplot2::theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x= element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  ggplot2::coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai) + 0.05), expand = FALSE)+
  ggplot2::ylab("Plant Area Veg. Density (PAVD)")+
  ggplot2::xlab("Height Above Ground (m)")+
  ggplot2::ggtitle(filename)+
  ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

ggplot2::ggsave(plot.file.path, hit.grid, width = 8, height = 6, units = c("in"))
} else {
  pavd <- ggplot2::ggplot(df.z, aes(y = df.z$ratio.vai, x = df.z$zbin))+
    #geom_bar(stat = "identity", color = "light grey")+
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, sf))+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.text.x= element_text(size = 14),
                   axis.text.y = element_text(size = 14),
                   axis.title.x = element_text(size = 20),
                   axis.title.y = element_text(size = 20))+
    ggplot2::coord_flip(xlim = NULL, ylim = c(0, max(df.z$ratio.vai) + 0.05), expand = FALSE)+
    ggplot2::ylab("Plant Area Veg. Density (PAVD)")+
    ggplot2::xlab("Height Above Ground (m)")+
    ggplot2::ggtitle(filename)+
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))
}


ggplot2::ggsave(plot.file.path.pavd, pavd, width = 8, height = 6, units = c("in"))

}

