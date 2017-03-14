#' Split transects from PCL
#'
#' \code{split_transects_from_pcl} imports and processes a single PCL transect.
#'
#' # Function to add two additional columns
#' # to the pcl dataset, one for the segment
#'  (which should only be from 1-4) and is
#' # designated by a -99999999 value in the
#' # return_distance column
#' # The only required parameter is the data
#' # frame of pcl data, but this can
#' # optionally also write out the results
#' # to csv if a path and name are given
#' @param pcl_data data frame of unprocessed PCL data
#' @param transect.length total transect length
#' @param marker.spacing distance between markers in meters within the PCL data
#' @param data_dir directory where PCL data .csv are stored
#' @param output_file_name old code relic
#'
#' @examples
#'
#' \dontrun{
#'
#' }
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
split_transects_from_pcl <- function(pcl_data, transect.length, marker.spacing, DEBUG = FALSE, write_out = FALSE, data_dir, output_file_name) {

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
    if (segment_num == ((transect.length/marker.spacing) + 1)) {
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
    stopifnot(max(results$seg_num) < ((transect.length/marker.spacing) + 1))
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
