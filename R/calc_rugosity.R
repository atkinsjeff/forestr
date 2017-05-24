#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{calc_rugosity} imports and processes a single PCL transect.
#'
#' This is a specific function calculates canopy rugosity and other metrics
#'
#' @param df is a data frame of adjusted pcl data.
#' @param m matrix of light adjusted vai values.
#' @param filename the name of the file currently being processed.
#'
#' @keywords rugosity
#' @return a slew of metrics...need to fully outline later
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

  mean.height = mean(df$height.bin)
  message("MeanHeight - plot mean of column mean return height")
  print(mean.height)

  height.2 <- stats::sd(df$height.bin)
  message("Standard Deviation of mean height for each xbin - height2")
  print(height.2)


  mode.el = mean(df$max.vai.z)
  message("Mean Height of Maximum Return Density - modeEl")
  print(mode.el)


  df$max.vai.sq <- df$max.vai.z^2
  mode.2 <- mean(df$max.vai.sq)
  mode.2 = (mode.2 - (mode.el * mode.el))^0.5
  message("Mean height of squared max VAI whatever the hell that is -- or mode2")
  print(mode.2)



  max.el = max(df$max.vai.z)
  message("Maximum VAI for entire transect -- max el!")
  print(max.el)



  max.can.ht = max(df$max.ht)
  message("Max canopy height (m)")
  print(max.can.ht)

  mean.max.ht = mean(df$max.ht)
  message("Mean Outer Canopy Height (m) or MOCH")
  print(mean.max.ht)

  mean.vai = mean(df$sum.vai)
  message("Mean VAI")
  print(mean.vai)

  message("Maximum VAI")
  max.vai = max(df$sum.vai)
  print(max.vai)

  #added the df thing in front of max ht, but don't know if it works.
  e <- subset(df, df$max.ht == 0)
  deep.gaps <- nrow(e)
  message("Deep Gaps")
  print(deep.gaps)

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
  std.std = mean(super.size$std.bin.squared)

  mean.std = mean(super.size$std.bin)


  message("Square of leaf height variance (stdStd from old script)")
  print(std.std)


  message("Mean Standard deviation of leaf heights -- meanStd")
  print(mean.std)

  rugosity = (std.std - mean.std * mean.std)^0.5
  message("Canopy Rugosity")
  print(rugosity)


  jess.rugosity = stats::sd(df$max.ht)
  message("Surface Rugosity--TopRugosity")
  print(jess.rugosity)

  variable.list <- list(plot = filename,
                        mean.height = mean.height,
                        transect.length = transect.length,
                        mode.el = mode.el,
                        height.2 = height.2,
                        max.el = max.el,
                        mode.2 = mode.2,
                        max.can.ht = max.can.ht,
                        mean.max.ht = mean.max.ht,
                        mean.vai = mean.vai,
                        max.vai = max.vai,
                        deep.gaps = deep.gaps,
                        porosity = porosity,
                        std.std = std.std,
                        mean.std = mean.std,
                        rugosity = rugosity,
                        top.rugosity = jess.rugosity)


  #now to write to csv
  variable.list <- data.frame(variable.list)
  return(variable.list)

}
