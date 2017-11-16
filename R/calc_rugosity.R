#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{calc_rugosity} calculates canopy structural complexity metrics from PCL data and prints them to the screen.
#'
#' This is a specific function calculates canopy rugosity and other metrics, including rumple, height metrics, etc.
#'
#' @param df is a data frame of adjusted pcl data.
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#'
#' @keywords rugosity, porosity, csc
#'
#' @return a series of metrics that describe canopy and ecosystem height, density, openness, cover, etc.
#'
#' @export
#' @examples
#' # Calculates metrics of canopy structural complexity.
#' \dontrun{calc_rugosity()
#'}


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

  mean.height = mean(df$height.bin)
  message("Mean Leaf Height (H) - plot mean of column mean leaf height")
  print(mean.height)

  height.2 <- stats::sd(df$height.bin)
  message("Height2 (H[2]) - standard deviation of column mean leaf height")
  print(height.2)

  mean.height.var = stats::var(df$height.bin)
  message("Mean Leaf Height variance (H[var]) - variance of column mean leaf height")
  print(mean.height.var)

  mean.height.rms = sqrt(mean(df$height.bin^2))
  message("Root Mean Square Mean Leaf Height (H[rms]) - the root mean square or quadratic mean of column mean leaf height for the transect")
  print(mean.height.rms)

  max.can.ht = max(df$max.ht)
  message("Max canopy height (m)")
  print(max.can.ht)

  mean.max.ht = mean(df$max.ht)
  message("Mean Outer Canopy Height (m) or MOCH")
  print(mean.max.ht)

  message("AREA AND DENSITY METRICS")

  mean.vai = mean(df$sum.vai)
  message("Mean VAI - mean VAI for entire transect")
  print(mean.vai)

  mode.el = mean(df$max.vai.z)
  message("Mean Height of VAI[max] - modeEl")
  print(mode.el)

  mode.2 <- stats::sd(df$max.vai.z)
  message("Mode 2- The standard deviation of VAImax or MaxEl")
  print(mode.2)

  max.el = max(df$max.vai)
  message("Maximum VAI for entire transect -- max el!")
  print(max.el)

  mean.peak.vai = mean(df$max.vai)
  message("Mean Peak VAI for entire transect")
  print(mean.peak.vai)

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
  std.std = ((mean(super.size$std.bin))^2) / transect.length

  # mean.std = mean(super.size$std.bin)
  mean.std = mean(super.size$std.bin) / transect.length

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
                        mean.height = mean.height,
                        height.2 = height.2,
                        mean.height.var = mean.height.var,
                        mean.height.rms = mean.height.rms,
                        transect.length = transect.length,
                        mode.el = mode.el,
                        max.el = max.el,
                        mode.2 = mode.2,
                        max.can.ht = max.can.ht,
                        mean.max.ht = mean.max.ht,
                        mean.vai = mean.vai,
                        mean.peak.vai = mean.peak.vai,
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
