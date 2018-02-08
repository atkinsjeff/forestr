#' Plots LiDAR hit grids of VAI
#'
#' \code{plot_hit_grid} produces a LiDAR hit grid plot
#'
#'
#'
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#' @param transect.length the length of the transect used to create the x-axis
#' @param max.ht the maximum measured height used to create the y-axis
#' @param max.vai the maximum density of VAI, defaul = 8
#' @keywords hitgrid, plot,
#' @return a hit gride of VAI
#'
#' @export
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' plot_hit_grid(pcl_vai, filename = "UVA LiDAR data", transect.length = 40,
#'  max.ht = 30, max.vai = 8)
#'


# PAVD script
plot_hit_grid <- function(m, filename, transect.length, max.ht, max.vai) {
  # m = vai matrix
  xbin <- NULL
  zbin <- NULL
  vai <- NULL
  m$vai[m$vai == 0] <- NA

  # Deal with missing stuff
  if(missing(filename)){
    filename = ""
  }

  # Deal with missing stuff
  if(missing(max.ht)){
    stop("Please provide maximum height, max.ht = ...")

  }

  if(missing(transect.length)){
    stop("Please provide transect length, transect.length = ...")
  }

  if(missing(max.vai)){
    message("Maximum VAI set at 8")
    max.vai = 8
  }

  vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  ggplot2::ggplot(m, ggplot2::aes(x = xbin, y = zbin))+
    ggplot2::geom_tile(ggplot2::aes(fill = vai))+
    ggplot2::scale_fill_gradient(low="gray88", high="dark green",
                                 na.value = "white",
                                 limits=c(0, max.vai),
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
    ggplot2::ylim(0,max.ht)+
    ggplot2::xlab("Distance along transect (m)")+
    ggplot2::ylab("Height above ground (m)")+
    ggplot2::ggtitle(filename)+
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

  #ggplot2::ggsave(plot.file.path.hg, hit.grid, width = 8, height = 6, units = c("in"))

}
