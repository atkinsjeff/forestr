#' Calculate rugosity and other higher level complexity metrics
#'BEGIN COMPLEXITY METRIC CALCS
#

# RUGOSITY
calc_rugosity <- function(df, m, filename) {
  #df = the summary matrix
  #m = vai matrix

  a <- subset(df, max.vai.z > 0)

  transect.length = max(df$xbin)
  message("Transect Length (m)")
  print(transect.length)

  mean.height = mean(df$height.bin)
  message("MeanHeight - plot mean of column mean return height")
  print(mean.height)

  height.2 <- sd(df$height.bin)
  message("Standard Deviation of mean height for each xbin - height2")
  print(height.2)


  mode.el = mean(df$max.vai.z)
  message("Mean Height of Maximum Return Density -- modeEl")
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
  message("Mean Max canopy height (m) -- meanTopel w/ deep gaps removed")
  print(mean.max.ht)

  mean.vai = mean(df$sum.vai)
  message("Mean VAI")
  print(mean.vai)

  message("Maximum VAI")
  max.vai = max(df$sum.vai)
  print(max.vai)

  e <- subset(df, max.ht == 0)
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

  j <- aggregate(std.bin.num ~ xbin, data = combo.meal, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
  j[is.na(j)] <- 0


  super.size <- merge(df, j, by = "xbin")

  super.size$std.bin <- super.size$std.bin.num / super.size$sum.vai

  super.size$std.bin.squared <- (super.size$std.bin^2)

  super.size[is.na(super.size)] <- 0
  #print(super.size)
  std.std = mean(super.size$std.bin.squared)
  #std.std = std.std/transect.length

  mean.std = mean(super.size$std.bin)
  #mean.std = mean.std/transect.length

  # super.size$std.std.pre <- (super.size$std.bin^2) / transect.length
  # super.size$std.std.pre[is.na(super.size$std.std.pre)] <- 0
  #
  # print(super.size)
  # std.std <- sum(super.size$std.std.pre)

  # super.size$std.std.pre[is.infinite(super.size$std.std.pre)] <- 0

  # super.size$mean.std.pre <- (super.size$std.bin / transect.length)
  # super.size$mean.std.pre[is.na(super.size$mean.std.pre)] <- 0
  #
  # # super.size$mean.std.pre[is.infinite(super.size$mean.std.pre)] <- 0
  # mean.std = sum(super.size$mean.std.pre)
  # print(super.size)
  # super.size$mean.std.pre <- super.size$std.bin / transect.length
  # mean.std = sum(super.size$mean.std.pre)


  message("Square of leaf height variance (stdStd from old script)")
  print(std.std)


  message("Mean Standard deviation of leaf heights -- meanStd")
  print(mean.std)

  rugosity = (std.std - mean.std * mean.std)^0.5
  message("Canopy Rugosity")
  print(rugosity)

  # #uses temp. data frame with deep gaps removed

  jess.rugosity = sd(df$max.ht)

  # sum(el_CP(CP(p)+k-1,:).*((z_CP(CP(p)+k-1,:)-heightBin).^2))/sum(el_CP(CP(p)+k-1,:))



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
