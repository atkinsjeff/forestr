###functions.R for LaserquestR project focused on calculating CSC metrics for LiDAR data



######################3
# Import PCL data function
read.pcl <- function(data_dir, filename) {
     f <- file.path(data_dir, filename)
     df <- read.csv(f, header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)      #was originally false
     df$index <- as.numeric(rownames(df))
     df = df[,c(3, 1, 2)]
     df
}

# this function accounts for the NAs that are in return distance which are actually the sky hits (i.e. when the lidar does not record a canopy hit)
#
code_hits <- function(df) {
     for(i in 1:nrow(df)){
          if (is.na(df$return_distance[i]) == TRUE) {
               df$sky_hit[i] = TRUE
               df$can_hit[i] = FALSE
               df$marker[i] = FALSE
          }else{
          if (df$return_distance[i] > 0){
               df$sky_hit[i] = FALSE
               df$can_hit[i] = TRUE
               df$marker[i] = FALSE
          }else{
               df$sky_hit[i] = FALSE
               df$can_hit[i] = FALSE
               df$marker[i] = TRUE

          }
     }
     }
     return(df)
}

adjust_by_user <- function(df, user.ht) {
     df$return_distance <- df$return_distance + user.ht
     df
}
# this function mirrors the read.table, read.csv function, but is written for pcl data

pcl.diagnostic.plot <- function(df, filename) {
     plot(df$index,
          df$return_distance,
          main = filename,

          ylab = "Canopy Height (m)",
          xlab = "")
}

get.transect.length <- function (df) {

     transect.length <- (length(which((df$return_distance <= -9999))) - 1) * 10
     return(transect.length)

}
##########################################
##########################################
# Function to read in a pcl text file and
# optionally print out a head and plot
# to check if it's ok.
#
# Expects a character string of a path to
# a data directory, a filename, and
# optionally a boolean DEBUG option
##########################################
##########################################
read_and_check_pcl <- function(data_dir, filename, DEBUG = FALSE) {

     # Load data
     pcl.in <- read.pcl(paste0(data_dir, filename))
     pcl.in <- add_sky_hits(pcl.in)
     # Sanity checks
     if (DEBUG) head(pcl.in)
     if (DEBUG) pcl.diagnostic.plot(pcl.in, "", 25)

     pcl.in
}


##########################################
##########################################
# Function to add two additional columns
# to the pcl dataset, one for the segment
# (which should only be from 1-4) and is
# designated by a -99999999 value in the
# return_distance column
# The only required parameter is the data
# frame of pcl data, but this can
# optionally also write out the results
# to csv if a path and name are given
##########################################
##########################################
split_transects_from_pcl <- function(pcl_data, transect.length, marker.distance, DEBUG = FALSE, write_out = FALSE, data_dir, output_file_name) {

     # Initialize count for segments (expecting 4 segments per transect)
     # Some returns before beginning of first segment and some after last
     segment_num <- 0

     # Check for how many segment boundaries we have (should be 5)
     #stopifnot(length(pcl_data[pcl_data$return_distance == -99999999, 2]) = 5)
    # stopifnot(length(which(pcl_data$return_distance < -9999)) == 4)


     # Walk through rows and add the segment number in a new column
     for (i in 1:nrow(pcl_data)) {
          pcl_data$seg_num[i] <- segment_num

          if(pcl_data$return_distance[i] <= -9999 & !is.na(pcl_data$return_distance[i])){
               segment_num <- segment_num + 1

          }
          if (segment_num == ((transect.length/marker.distance) + 1)) {
               break
          }
     }

     # Check to see if it worked
     if (DEBUG) head(pcl_data)

     # Initialize empty data frame to store results
     results <- data.frame()

     # For each segment there should only be 4 in total -- checked with test
     # but we're flexible here. Uses cut() with labels = FALSE to return

     #### this needs to be adjusted to account for smaller transects
     # a vector of integer categories for each "chunk" within each segment
     # This should go from 1-10 and be spaced evenly in "index" space
for (i in 1:(max(pcl_data$seg_num) - 1)) {
      for (i in 1:(max(pcl_data$seg_num))) {
          this_segment <- subset(pcl_data, pcl_data$seg_num == i)
          this_segment$chunk_num <- cut(this_segment$index, 10, labels = FALSE)
          results <- rbind(results, this_segment)
     }

     # Make sure we didn't make too many chunks in any segment
     stopifnot(max(results$chunk_num) < 11)
     stopifnot(max(results$seg_num) < ((transect.length/marker.distance) + 1))
}
     # Code segment to create zbin and xbin
     results$xbin <- ((results$seg_num * 10) - 10)  + results$chunk_num
     results$zbin <- round(results$return_distance)
     results$zbin[results$sky_hit == "TRUE"] <- 0
     # Check final output
     if (DEBUG) head(results)
     if (DEBUG) tail(results)

     # Write out if write parameter is set at top
     if (write_out) write.csv(results, paste0(data_dir, output_file_name, ".with_categories.csv"), row.names = FALSE)

     results <- distinct(results, index, .keep_all = TRUE)
     results
}


#####Canopy metrics before matrix creations

csc_metrics <- function(df, filename) {
     z <- df
     z <- subset(z, return_distance >= 0)

     #for the cover fraction calculation, number of markers - 1
     correction.coef <- length(which((df$return_distance <= -9999)))

     mean.return.ht = mean(z$return_distance, na.rm = TRUE)
     message("Mean Return Height (m) -- meanHeight in old code")
     print(mean.return.ht)

     sd.ht = sd(z$return_distance, na.rm = TRUE)
     message("Standard Deviation of Canopy Height returns-- meanStd in old code")
     print(sd.ht)

     sky.fraction = (1 - (length(which(df$can_hit == TRUE)) / (length(df$return_distance) - correction.coef) )) * 100
     message("Sky Fraction (%)")
     print(sky.fraction)

     cover.fraction = 100 - sky.fraction
     message("Cover Fraction (%)")
     print(cover.fraction)

     max.ht = max(df$return_distance, na.rm = TRUE)
     message("Max Measured Canopy Height (%)")
     print(max.ht)

     csc.variable.list <- list(plot = filename,
                               mean.return.ht = mean.return.ht,
                               sd.return.ht = sd.ht,
                               sky.fraction = sky.fraction,
                               cover.fraction = cover.fraction,
                               max.ht = max.ht)
     csc.variable.list <- data.frame(csc.variable.list)
     return(csc.variable.list)
}

##########################################
##########################################
# This section creates the matrix that is
# required to claculate what we need
##########################################
##########################################
##########################################
##########################################


make_matrix_part_one <- function(df) {
     #ultimately this should actually make an empty data frame or something
     #and it should go from x 1:40 and z to whatever so there are empty values in there
     z = df
     #z <- subset(z, return_distance >= 0)
     # zz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = mean), c("xbin", "mean.ht"))
     # zzz <-setNames(aggregate(return_distance ~ xbin, data = z, FUN = sd), c("xbin", "sd.ht"))
     # zzzz <- setNames(aggregate(return_distance ~ xbin, data = z, FUN = max), c("xbin", "max.ht"))

     # # number of lidar returns for entire column
     # l <- setNames(aggregate(index ~ xbin, data = df, FUN = length), c("xbin", "lidar.pulses"))
     #print(l)
     # number of return per x,z bin in the canopy
     m <- setNames(aggregate(return_distance ~ xbin + zbin, data = df, FUN = length), c("xbin", "zbin","bin.hits"))
     m <- m[!m$zbin < 0, ]

     # number of sky.hits per column (x)
     n <- setNames(aggregate(sky_hit ~ xbin, data = df, FUN = sum), c("xbin", "sky.hits"))

     # number of canopy returns in column
     k <- setNames(aggregate(can_hit ~ xbin, data = df, FUN = sum), c("xbin", "can.hits"))


     #print(k)
     #p <- Reduce(function(x, y) merge(x,y, all = TRUE), list(m, l, n, k))
      p <- merge(m, n, by = c("xbin"), all = TRUE)
      p <- merge(p, k, by = c("xbin"), all = TRUE)

      p$lidar.pulses <- p$can.hits + p$sky.hits
     # p <- merge(p, zz, by = c("xbin"), all = TRUE)
     # p <- merge(p, zzz, by = c("xbin"), all = TRUE)
     # p <- merge(p, zzzz, by = c("xbin"), all = TRUE)
     replace(p, is.na(p), 0)#This will correct for any gaps w/out msmts as all NAs will be 0


}


make_matrix_part_two <- function(df) {
     #ultimately this should actually make an empty data frame or something
     p <- df

     df2 <- expand.grid(xbin = c(1:max((p$xbin))),
                       zbin = c(0:max((p$zbin))))

     #
     q <- merge(p, data.frame(table(df2[1:2])), all.y=TRUE)
     #now to add empty rows as NA
     #q <- merge(p, data.frame(table(p[1:2]))[-c(3:9)],all.y=TRUE)
     replace(q, is.na(q), 0)#This will correct for any gaps w/out mesmts as all NAs will be 0

}

# this command combines the previous functions
make_matrix <- function(df) {
     df <- make_matrix_part_one(df)
     df <- make_matrix_part_two(df)
     df$xbin <- as.integer(as.character(df$xbin))
     df$zbin <- as.integer(as.character(df$zbin))

     k <- setNames(aggregate(can.hits ~ xbin, data = df, FUN = max), c("xbin", "can.hits"))
     df$can.hits <- k$can.hits[match(df$xbin, k$xbin)]

     l <- setNames(aggregate(lidar.pulses ~ xbin, data = df, FUN = max), c("xbin", "lidar.pulses"))
     df$lidar.pulses <- l$lidar.pulses[match(df$xbin, l$xbin)]

     return(df)
}


#### light saturation correction
normalize_pcl_one <-  function(df) {
     # for loop for this jenk
     # what we are doing is counting up the number of canopy hits to an x,z point in the canopy

     # first we sort
     df <- df[with(df, order(xbin, zbin)), ]

     df$hit.count <- 0   #creates and empty column of zeros
     for (i in 1:nrow(df)) {
          x.counter = 1  #a counter! woohoo

          for(j in 2:nrow(df)){
               if(df$xbin[j] == x.counter ){

                    df$hit.count[j] = df$hit.count[j-1] + df$bin.hits[j]

               }else {
                    x.counter = x.counter + 1
                    next
               }
               next
          }
     }
     return(df)
}

normalize_pcl_two <- function(df) {

     eq1 = (df$can.hits - df$hit.count) / df$can.hits
     eq2 = ((df$can.hits + 1) - df$hit.count) / (df$can.hits + 1)

     # if can.hits and lidar.pulses are equal, then canopy is saturated

     df <- transform(df, phi = ifelse(lidar.pulses == can.hits, eq1, eq2))
}

normalize_pcl_three <- function(df) {

     df$dee <- 0   #creates and empty column of zeros
     for (i in 1:nrow(df)) {
          x.counter = 1  #a counter! woohoo

          for(j in 2:nrow(df)){
               if(df$phi[j-1] > 0 && df$phi[j] > 0 && df$xbin[j] == x.counter ){

                    df$dee[j] = log(df$phi[j-1] / df$phi[j])

               }else {
                    df$dee[j] = 0
                    x.counter = x.counter + 1
                    next
               }
               next
          }
     }

     # now to sum up dee to make sum.dee the %total adjusted hits in the column
     q <- setNames(aggregate(dee ~ xbin, data = df, FUN = sum), c("xbin", "sum.dee"))
     df$sum.dee <- q$sum.dee[match(df$xbin, q$xbin)]




     # now to make fee a percentage of the percent hits at that level
     eq.fee = df$dee / df$sum.dee

     # for all columns where dee is >0 i.e. that is saturated
     df <- transform(df, fee = ifelse(sum.dee > 0, eq.fee, 0))
     return(df)
}
     #
     # if(df$lidar.pulses == df$can.hits){
     #      df$phi = (df$can.hits - df$hit.count) / df$can.hits
     # }else{
     #      df$phi = ((df$can.hits + 1) - df$hit.count) / (df$can.hits + 1)
     # }


####
# # asum=sum(d); %Total hits in a bin
# NoSky = find(asum>0.0);
# phi(1,NoSky)=1.0;
# for ci =2:nvertbins
# c(ci,NoSky) = d(ci-1,NoSky)+c(ci-1,NoSky); % Next bin down of c + bins below it, so total hits that have happened by that point
# saturated = find(c(ci,NoSky)>=asum(NoSky)); %Find when all the pulses have hit (note that this is only for transects which have no sky hits)
# phi(ci,NoSky)=(asum(NoSky)-c(ci,NoSky))./asum(NoSky) ; %Percent of saturation at a given height
# phi(ci,NoSky(saturated))=(asum(NoSky(saturated))+1-c(ci,NoSky(saturated)))./(asum(NoSky(saturated))+1) ;
# nonzero = find(phi(ci-1,:)>=0.00000001 & phi(ci,:)>=0.00000001 ); %
# dee(ci-1,nonzero)=log(phi(ci-1,nonzero)./phi(ci,nonzero)); %Ln of the saturation perc for the bin below divided by current bin
# end %for ci
# sumdee = sum(dee); %total adjusted hits in a column
# %nonzero=find(sumdee>0.000000001);
# for ch =1:binmax
# if sumdee(ch)>0.000000001 %if more than 0 adjusted hits
# fee(:,ch)=dee(:,ch)./sumdee(ch); %fee is a perc. of the percent hits at that level
# end
# end
#
# cvr1=find(cvr<0.999999999999); %Anything that is not perfectly saturated

#####this series of functions creates VAI
calc_vai <- function(df) {

     # this should be how much cover (cvr) is in each, x,z bin index value
     df$cvr <- (df$bin.hits / df$can.hits)

     # olai has a maxium value of 8. eq one is for use on areas that are not saturated
     eq.olai = (log(1.0 - (df$can.hits/df$lidar.pulses)*0.9817)  * -1) /0.5

     ##### you need to do this for all of those columns! olai by column
     # for all columns that are less than saturated, adjust olai
     df <- transform(df, olai = ifelse(lidar.pulses > can.hits, eq.olai, 8))

     # now make adjusted vai
     eq.vai1 = df$olai * df$fee
     eq.vai2 = df$olai * df$cvr
     df <- transform(df, vai = ifelse(fee > 0,  eq.vai1, eq.vai2 ))

     # df[is.na(df)] <- 0


     # df$vai <- (log(1.0 - df$cvr*0.9817)  * -1) /0.5
     df[is.na(df)] <- 0
     return(df)

}



#############################
#############################
#############################
## Need to make a summary matrix now based on each column

# This makes a dataframe that is as long as a transect is. If the transect is 40 m, this data frame has 40 rows.

make_summary_matrix <- function(df, m) {

     df <- subset(df, return_distance > 0)

     # mean height
     a <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = mean, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "mean.ht"))

     # standard deviation of column height
     b <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.ht"))

     # max height in column
     c <- setNames(aggregate(return_distance ~ xbin, data = df, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.ht"))

     # maximum value of VAI in the column
     d <- setNames(aggregate(vai ~ xbin, data = m, FUN = max, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "max.vai"))

     # sum of VAI in column
     e <- setNames(aggregate(vai ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.vai"))

     # standard deviation of VAI for column
     f <- setNames(aggregate(vai ~ xbin, data = m, FUN = sd, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sd.vai"))

     # this is height at which max vai occurs
     g <- m$zbin[match(d$max.vai, m$vai)]
     g <- data.frame(g)
     colnames(g) <- c("max.vai.z")
     #print(g)

     #mean column leaf height that is the "heightBin" from Matlab code
     # first we make el
     #m$el <- (m$vai / m$sum.vai) * 100
     m$vai.z <- m$vai * (m$zbin +0.5)
     h <- setNames(aggregate(vai.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "vai.z.sum"))


     # this section joins all these guys together
     p <- join_all(list(a, b, c, d, e, f, h), by = "xbin", type = "full")
     p <- p[with(p, order(xbin)), ]
     p <- cbind(p, g)




     p$mean.ht[is.na(p$mean.ht)] <- 0
     p$sd.ht[is.na(p$sd.ht)] <- 0
     p$max.ht[is.na(p$max.ht)] <- 0

     p$height.bin <- p$vai.z.sum / p$sum.vai
     p[is.na(p)] <- 0
     # p$std.bin.num <- p$vai * ((p$zbin - p$height.bin)^2)
     #
     # j <- aggregate(std.bin.num ~ xbin, data = p, FUN = sum, na.rm = FALSE, na.action = 'na.pass')
     # j[is.na(j)] <- 0
     # print(j)
     #
     # p <- merge(p, j, by = "xbin")
     # p$std.bin <- p$std.bin.num / p$sum.vai
     # first we sort

     return(p)
}

calc_tls_vai <- function(m) {
     m$vai <- (log(1.0 - (m$vai)*0.9817)  * -1) /0.5
     return(m)
}

calc_tls_mean_leaf_ht <- function(m){
     #mean column leaf height that is the "heightBin" from Matlab code

     m$vai.z <- m$vai * (m$zbin +0.5)
     h <- setNames(aggregate(vai.z ~ xbin, data = m, FUN = sum, na.rm = FALSE,  na.action = 'na.pass'), c("xbin", "vai.z.sum"))

     e <- setNames(aggregate(vai ~ xbin, data = m, FUN = sum, na.rm = FALSE, na.action = 'na.pass'), c("xbin", "sum.vai"))

     # this section joins all these guys together
     p <- join_all(list(m, e, h), by = "xbin", type = "full")


     p$height.bin <- p$vai.z.sum / p$sum.vai
     return(p)

}

calc_tls_std_bin <- function(m){
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


     message("Square of leaf height variance (stdStd from old script)")
     print(std.std)


     message("Mean Standard deviation of leaf heights -- meanStd")
     print(mean.std)

     rugosity = (std.std - mean.std * mean.std)^0.5
     message("Canopy Rugosity")
     print(rugosity)

}

calc_rumple <- function(df){
     df$rump.diff <- 0

         for (i in 2:nrow(df)) {

         df$rump.diff[i] <- abs(ceiling(df$max.ht[i - 1]) - ceiling(df$max.ht[i]))

         }
     #print(df$rump.diff)
     rumple = (sum(df$rump.diff) + max(df$xbin)) / max(df$xbin)
     message("Rumple")
     print(rumple)
     return(rumple)
}

calc_gap_fraction <- function(m){
     transect.length <- max(m$xbin)

     for(i in 1:nrow(m)){
          if (m$bin.hits[i] == 0) {
               m$gap[i] = 1
          }else{
               m$gap[i] = 0
               }
          }
     # the thinking here is that you can average across the z plane to get the gap fraction, or the portion of sky/canopy unobstructed by canopy. so if four pixels out of five were empty, that would be 0.8 gap fraction  or the mean(c(1,1,1,1,0))--think of the 1 as gap = true
     #print(m$gap)
     gap.list <- setNames(aggregate(gap ~ zbin, data = m, FUN = mean, na.rm = FALSE, na.action = 'na.pass'), c("zbin", "gap.fraction"))
     #print(gap.list)

     mean.gap.fraction = mean(gap.list$gap.fraction)
     message("Mean Gap Fraction ---as error check should be same as porosity")
     print(mean.gap.fraction)

     message("now we replace the 0's with 1's so when we take the ln they = 0")
     gap.list[gap.list == 0] <- 1

     #print(gap.list)
     gap.list$ln.gap.fraction <- log(gap.list$gap.fraction)

     #print(gap.list)
     clump <- log(mean.gap.fraction) / mean(gap.list$ln.gap.fraction)
     message("Clumping Index")
     print(clump)
     return(clump)

    }
##########################
##########################
# BEGIN COMPLEXITY METRIC CALCS
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

combine_variables <- function(variable.list, csc.metrics, rumple, clumping.index){

     output.variables <- cbind(variable.list, csc.metrics, rumple, clumping.index)
     return(output.variables)

     }

write.pcl.to.csv <- function(output.variables, filename) {

     filename2 <- paste(filename, ".csv", sep="")
     write.csv(output.variables, file.path(output_directory, filename2))
}

write.summary.matrix.to.csv <- function(m, filename) {

     filename2 <- paste(filename, "_summary_matrix.csv", sep="")
     write.csv(m, file.path(output_directory, filename2))
}

write.hit.matrix.to.csv <- function(m, filename) {
     m <- m[, c("xbin", "zbin", "vai")]

     filename2 <- paste(filename, "_hit_matrix.csv", sep="")
     write.csv(m, file.path(output_directory, filename2))
}


process.single.transect <- function(data_dir, filename){

     write_out <- FALSE


     test.data <- read.pcl(data_dir, filename)
     transect.length <- get.transect.length(test.data)
     test.2 <- code_hits(test.data)


     #adjusts by the height of the  user to account for difference in laser height to ground
     test.2 <- adjust_by_user(test.2, 1.2)

     # need to code in diagnostic plot better
     #pcl.diagnostic.plot(test.2, filename)

     test.data.binned <- split_transects_from_pcl(test.2, transect.length, 10)

     csc.metrics <- csc_metrics(test.data.binned, filename)

     m1 <- make_matrix(test.data.binned)

     m2 <- normalize_pcl_one(m1)
     m3 <- normalize_pcl_two(m2)
     m4 <- normalize_pcl_three(m3)

     m5 <- calc_vai(m4)


     summary.matrix <- make_summary_matrix(test.data.binned, m5)
     rumple <- calc_rumple(summary.matrix)
     clumping.index <- calc_gap_fraction(m5)

     variable.list <- calc_rugosity(summary.matrix, m5, filename)

     output.variables <- combine_variables(variable.list, csc.metrics, rumple, clumping.index)
     print(output.variables)

     outputname = substr(filename,1,nchar(filename)-4)
     outputname <- paste(outputname, "output", sep = "_")
     dir.create("output", showWarnings = FALSE)
     output_directory <- "./output/"

     write.pcl.to.csv(output.variables, outputname)
     write.summary.matrix.to.csv(summary.matrix, outputname)
     write.hit.matrix.to.csv(m5, outputname)




     #get filename first
     plot.filename <- file_path_sans_ext(filename)

     plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

     vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
     x11(width = 8, height = 6)
     hit.grid <- ggplot(m5, aes(x = xbin, y = zbin))+
          geom_tile(aes(fill = vai))+
          scale_fill_gradient(low="white", high="dark green",
                              limits=c(0,8.5),
                              name=vai.label)+
          #scale_y_continuous(breaks = seq(0, 20, 5))+
          # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
          theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text.x= element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20))+
          xlim(0,transect.length)+
          ylim(0,41)+
          xlab("Distance along transect (m)")+
          ylab("Height above ground (m)")+
          ggtitle(filename)+
          theme(plot.title = element_text(lineheight=.8, face="bold"))

     ggsave(plot.file.path, hit.grid)
}

process.multiple.transects <- function(data_dir){

     file.names <- dir(data_dir, pattern =".CSV")
     length(file.names)

     for(i in 1:length(file.names)){
          filename <- file.names[i]

          write_out <- FALSE


          test.data <- read.pcl(data_dir, filename)
          transect.length <- get.transect.length(test.data)
          test.2 <- code_hits(test.data)


          #adjusts by the height of the  user to account for difference in laser height to ground
          test.2 <- adjust_by_user(test.2, 1.2)

          # need to code in diagnostic plot better
          #pcl.diagnostic.plot(test.2, filename)

          test.data.binned <- split_transects_from_pcl(test.2, transect.length, 10)

          csc.metrics <- csc_metrics(test.data.binned, filename)

          m1 <- make_matrix(test.data.binned)

          m2 <- normalize_pcl_one(m1)
          m3 <- normalize_pcl_two(m2)
          m4 <- normalize_pcl_three(m3)

          m5 <- calc_vai(m4)


          summary.matrix <- make_summary_matrix(test.data.binned, m5)
          rumple <- calc_rumple(summary.matrix)
          clumping.index <- calc_gap_fraction(m5)

          variable.list <- calc_rugosity(summary.matrix, m5, filename)

          output.variables <- combine_variables(variable.list, csc.metrics, rumple, clumping.index)
          print(output.variables)

          outputname = substr(filename,1,nchar(filename)-4)
          outputname <- paste(outputname, "output", sep = "_")
          dir.create("output", showWarnings = FALSE)
          output_directory <- "./output/"

          write.pcl.to.csv(output.variables, outputname)
          write.summary.matrix.to.csv(summary.matrix, outputname)
          write.hit.matrix.to.csv(m5, outputname)




          #get filename first
          plot.filename <- file_path_sans_ext(filename)

          plot.file.path <- file.path(paste(output_directory, plot.filename, ".png", sep = ""))

          vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
          x11(width = 8, height = 6)
          hit.grid <- ggplot(m5, aes(x = xbin, y = zbin))+
               geom_tile(aes(fill = vai))+
               scale_fill_gradient(low="white", high="dark green",
                                   limits=c(0,8.5),
                                   name=vai.label)+
               #scale_y_continuous(breaks = seq(0, 20, 5))+
               # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
               theme(axis.line = element_line(colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.text.x= element_text(size = 14),
                     axis.text.y = element_text(size = 14),
                     axis.title.x = element_text(size = 20),
                     axis.title.y = element_text(size = 20))+
               xlim(0,transect.length)+
               ylim(0,41)+
               xlab("Distance along transect (m)")+
               ylab("Height above ground (m)")+
               ggtitle(filename)+
               theme(plot.title = element_text(lineheight=.8, face="bold"))

          ggsave(plot.file.path, hit.grid)
     }
}



