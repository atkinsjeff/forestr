#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{calc_rugosity} calculates canopy structural complexity
#' metrics from PCL data and prints them to the screen.
#'
#' This is a specific function calculates canopy rugosity
#' and other metrics, including rumple, height metrics, etc.
#'
#' @param df is a LiDAR summary matrix data frame
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#'
#' @keywords complexity
#'
#' @return a series of metrics that describe canopy and
#' ecosystem height, density, openness, cover, etc.
#'
#' @export
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' calc_rugosity(pcl_summary, pcl_vai, filename = "")
#'


# RUGOSITY
calc_rugosity <- function(df, m, filename) {
  #df = the summary matrix
  #m = vai matrix

  # Setting global variables
  max.vai.z  <-NULL
  max.ht <- NULL


  transect.length = max(df$xbin)
  message("Transect Length (m)")
  print(transect.length)

  # HEIGHT VARIABLES
  message("HEIGHT METRICS")

  mean.height.mean = mean(df$height.bin)
  message("Mean Leaf Height (H) - plot mean of column mean leaf height")
  print(mean.height.mean)

  height.2 <- stats::sd(df$height.bin)
  message("Height2 (H[2]) - standard deviation of column mean leaf height")
  print(height.2)

  mean.height.median <- stats::median(df$height.bin)
  message("H [median] - median column mean leaf height")
  print(mean.height.median)

  mean.height.var = stats::var(df$height.bin)
  message("Mean Leaf Height variance (H[var]) - variance of column mean leaf height")
  print(mean.height.var)

  mean.height.rms = sqrt(mean(df$height.bin^2))
  message("Root Mean Square Mean Leaf Height (H[rms]) - the root mean square or quadratic mean of column mean leaf height for the transect")
  print(mean.height.rms)

  can.max.ht = max(df$max.ht)
  message("Max canopy height (m)")
  print(can.max.ht)

  moch = mean(df$max.ht)
  message("Mean Outer Canopy Height (m) or MOCH")
  print(moch)

  can.max.ht.median = stats::median(df$max.ht)
  message("Median Column Max Ht ")
  print(can.max.ht.median)

  message("AREA AND DENSITY METRICS")

  vai.mean = mean(df$sum.vai)
  message("Mean VAI - mean VAI for entire transect")
  print(vai.mean)

  vai.sd = stats::sd(df$sum.vai)
  message("SD VAI - SD of VAI for entire transect")
  print(vai.sd)

  vai.median = stats::median(df$sum.vai)
  message("Median VAI - median VAI for entire transect")
  print(vai.median)

  vai.column.max = max(df$sum.vai)
  message("Maximum VAI x,y entire transect -- max el!")
  print(vai.column.max)

  vai.max.ht.mean = mean(df$max.vai.z)
  message("Mean Height of VAI[max] - modeEl")
  print(vai.max.ht.mean)

  vai.max.ht.sd <- stats::sd(df$max.vai.z)
  message("Mode 2- The standard deviation of VAImax or MaxEl")
  print(vai.max.ht.sd)

  vai.max.ht.median = stats::median(df$max.vai.z)
  message("Median of VAI max or Maxel")
  print(vai.max.ht.median)

  vai.max = max(df$max.vai)
  message("Maximum VAI x,y entire transect -- max el!")
  print(vai.max)

  vai.mean.peak = mean(df$max.vai)
  message("Mean Peak VAI for entire transect")
  print(vai.mean.peak)

  vai.peak.sd = stats::sd(df$max.vai)
  message("SD of peak VAI for entire transect")
  print(vai.peak.sd)

  vai.peak.median = stats::median(df$max.vai)
  message("Median Peak VAI for entire transect")
  print(vai.peak.median)



  #added the df thing in front of max ht, but don't know if it works.
  message("CANOPY AND OPENNESS METRICS (cont.)")
  e <- subset(df, df$max.ht == 0)
  deep.gaps <- nrow(e)
  message("Deep Gaps")
  print(deep.gaps)

  deep.gap.fraction <- deep.gaps/transect.length
  message("Deep Gap Fraction (0-1)")
  print(deep.gap.fraction)

  message("ARRANGEMENT METRICS")
  porosity = sum(m$bin.hits == 0) / length(m$bin.hits)
  message("Canopy porosity")
  print(porosity)

  #being rugosity intermediates

  #first we adjust the vai at each x,z by the z height of the bin
  combo.meal <- merge(df, m, by = "xbin")

  combo.meal$std.bin.num <- combo.meal$vai * (((combo.meal$zbin + 0.5)  - combo.meal$height.bin)^2)

  j <- stats::aggregate(std.bin.num ~ xbin, data = combo.meal, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
  j[is.na(j)] <- 0


  super.size <- merge(df, j, by = "xbin")

  super.size$std.bin <- super.size$std.bin.num / super.size$sum.vai

  super.size$std.bin.squared <- (super.size$std.bin^2)

  super.size[is.na(super.size)] <- 0

  # std.std = mean(super.size$std.bin.squared)
  std.std = mean(super.size$std.bin.squared)

  # mean.std = mean(super.size$std.bin)
  mean.std = mean(super.size$std.bin)

  message("Square of leaf height variance (stdStd from old script)")
  print(std.std)


  message("Mean Standard deviation of leaf heights -- meanStd")
  print(mean.std)

  rugosity = (std.std - (mean.std * mean.std))^0.5
  message("Canopy Rugosity")
  print(rugosity)


  jess.rugosity = stats::sd(df$max.ht)
  message("Surface Rugosity--TopRugosity")
  print(jess.rugosity)

    variable.list <- list(plot = filename,
                        mean.height.mean = mean.height.mean,
                        height.2 = height.2,
                        mean.height.median = mean.height.median,
                        mean.height.var = mean.height.var,
                        mean.height.rms = mean.height.rms,
                        transect.length = transect.length,
                        can.max.ht = can.max.ht,
                        moch = moch,
                        can.max.ht.median = can.max.ht.median,
                        vai.mean = vai.mean,
                        vai.sd = vai.sd,
                        vai.median = vai.median,
                        vai.column.max = vai.column.max,
                        vai.max.ht.mean = vai.max.ht.mean,
                        vai.max.ht.sd = vai.max.ht.sd,
                        vai.max.ht.median = vai.max.ht.median,
                        vai.max = vai.max,
                        vai.mean.peak = vai.mean.peak,
                        vai.peak.sd = vai.peak.sd,
                        vai.peak.median = vai.peak.median,
                        deep.gaps = deep.gaps,
                        deep.gap.fraction = deep.gap.fraction,
                        porosity = porosity,
                        std.std = std.std,
                        mean.std = mean.std,
                        rugosity = rugosity,
                        top.rugosity = jess.rugosity)


  #now to write to csv
  variable.list <- data.frame(variable.list)
  return(variable.list)

}
