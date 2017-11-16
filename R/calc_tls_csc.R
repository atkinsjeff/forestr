#' Calculates rumple
#'
#' \code{calc_tls_csc} calculates canopy structural complexity metrics from the tls vai matrix
#'
#' This is a specific function to calculate canopy structural complexity or CSC metrics from the VAI matrix imported in.
#'
#'
#' @param m matrix of vai data with mean leaf height column
#' @param filename the name of the file being process0
#'
#' @keywords csc rugosity tls
#' @return csc metrics
#' @examples
#' \dontrun{
#' calc_tls_csc(m)
#' }

calc_tls_csc <- function(m, filename){
  df <- m
  # first we create the std.bin numerator
  df$std.bin.num <- ((df$zbin - df$height.bin)^2) * df$vai

  j <- aggregate(std.bin.num ~ xbin, data = df, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
  #print(j)
  j[is.na(j)] <- 0

  super.size <- merge(m, j, by = "xbin")
  #print(super.size[5,])

  super.size$std.bin <- super.size$std.bin.num / super.size$sum.vai

  super.size$std.bin.squared <- (super.size$std.bin^2)

  super.size[is.na(super.size)] <- 0
  #print(super.size)

  std.std = mean(super.size$std.bin.squared)
  #std.std = std.std/transect.length

  mean.std = mean(super.size$std.bin)
  #mean.std = mean.std/transect.length



  # HEIGHT VARIABLES
  message("HEIGHT METRICS")

  mean.height = mean(m$height.bin)
  message("Mean Leaf Height (H) - plot mean of column mean leaf height")
  print(mean.height)


  height.2 <- stats::sd(m$height.bin)
  message("Height2 (H[2]) - standard deviation of column mean leaf height")
  print(height.2)

  mean.height.var = stats::var(m$height.bin)
  message("Mean Leaf Height variance (H[var]) - variance of column mean leaf height")
  print(mean.height.var)

  mean.height.rms = sqrt(mean(m$height.bin^2))
  message("Root Mean Square Mean Leaf Height (H[rms]) - the root mean square or quadratic mean of column mean leaf height for the transect")
  print(mean.height.rms)


  message("AREA AND DENSITY METRICS")

  mean.vai = mean(m$sum.vai)
  message("Mean VAI - mean VAI for entire transect")
  print(mean.vai)

  mode.el = mean(m$max.vai.z)
  message("Mean Height of VAI[max] - modeEl")
  print(mode.el)

  mode.2 <- stats::sd(m$max.vai.z)
  message("Mode 2- The standard deviation of VAImax or MaxEl")
  print(mode.2)

  max.el = max(m$max.vai)
  message("Maximum VAI for entire transect -- max el!")
  print(max.el)

  mean.peak.vai = mean(m$max.vai)
  message("Mean Peak VAI for entire transect")
  print(mean.peak.vai)

  message("ARRANGEMENT METRICS")
  porosity = sum(m$vai == 0) / length(m$vai)
  message("Canopy porosity")
  print(porosity)

  message("HETEROGENEITY METRICS")


  message("Square of leaf height variance (stdStd from old script)")
  print(std.std)


  message("Mean Standard deviation of leaf heights -- meanStd")
  print(mean.std)

  rugosity = (std.std - mean.std * mean.std)^0.5
  message("Canopy Rugosity")
  print(rugosity)


  variable.list <- list(plot = filename,
                        mean.height = mean.height,
                        height.2 = height.2,
                        mean.height.var = mean.height.var,
                        mean.height.rms = mean.height.rms,
                        mode.el = mode.el,
                        max.el = max.el,
                        mode.2 = mode.2,
                        mean.vai = mean.vai,
                        mean.peak.vai = mean.peak.vai,
                        porosity = porosity,
                        std.std = std.std,
                        mean.std = mean.std,
                        rugosity = rugosity)


  #now to write to csv
  variable.list <- data.frame(variable.list)
  return(variable.list)



}
