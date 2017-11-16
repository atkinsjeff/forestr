#' Process single PCL transects.
#'
#' \code{calc_tls_mean_leaf_ht} used in process_tls to calculate mean leaf height from tls slife
#'
#' This function derives mean leaf height from x, z vai from TLS data.
#'
#'
#' @param m  the vai matrix

#' @return adds columns to the matrix of height.bin
#'
#' @keywords tls processing
#' @export
#'
#'
#' @examples
#'
#' # with designated file
#' \dontrun{process_pcl("pcl_data.csv", marker.spacing = 10, user_height = 1.05, max.vai = 8)
#' }
#'
#' # with data frame
#' process_pcl(osbs, marker.spacing = 10, user_height = 1.05, max.vai = 8)
#'
#'
calc_tls_mean_leaf_ht <- function(m){
  #mean column leaf height that is the "heightBin" from Matlab code

  m$vai.z <- m$vai * (m$zbin +0.5)
  h <- stats::setNames(stats::aggregate(vai.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "vai.z.sum"))

  e <- stats::setNames(stats::aggregate(vai ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.vai"))

  # this section joins all these guys together
  p <- plyr::join_all(list(m, e, h), by = "xbin", type = "full")


  p$height.bin <- p$vai.z.sum / p$sum.vai

  #### Addtional summary data
  # c <- stats::setNames(stats::aggregate(return_distance ~ xbin, data = df, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.ht"))

  # maximum value of VAI in the column
  d <- stats::setNames(stats::aggregate(vai ~ xbin, data = m, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.vai"))

  # standard deviation of VAI for column
  f <- stats::setNames(stats::aggregate(vai ~ xbin, data = m, FUN = stats::sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.vai"))

  # this is height at which max vai occurs
  g <- m$zbin[match(d$max.vai, m$vai)]

  g <- data.frame(g)

  colnames(g) <- c("max.vai.z")

  q <- plyr::join_all(list(p, d, f), by = "xbin", type = "full")

  q <- q[with(q, order(xbin)), ]
  q <- cbind(q, g)


  return(q)

}
