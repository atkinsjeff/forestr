#' PCL transect from Ordway-Swisher Biological Station, Florida, US.
#'
#' A dataset that consists of one 40 m transect taken in a longleaf pine-oak savanna
#' in North-central Florida. Data collected April, 2016 by J. Atkins and R. Fahey.
#'
#' @format A data frame with 10506 rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#' }
#' @source \url{http://atkinsjeff.github.io}
"osbs"

#' PCL transect from a red pine plantation in Northern Michigan, US.
#'
#' A dataset that consists of one 40 m transect taken in a red pine plantations
#' in Northern Michigan. Data collected July, 2017 by J. Atkins.
#'
#' @format A data frame with 17559 rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#' }
#' @source \url{http://atkinsjeff.github.io}
"red_pine"

#' a data frame of vegetation area index (VAI)
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 1120 rows:
#' \describe{
#'   \item{.id}{column numbering}
#'   \item{xbin}{x-bin position}
#'   \item{zbin}{z-bin position}
#'   \item{bin.hits}{number of LiDAR returns at each x- and z- bin}
#'   \item{sky.hits}{total numer of sky hits per x column}
#'   \item{can.hits}{total numer of canopy hits per x column}
#'   \item{lidar.pulses}{no. of lidar pulses emitted per column}
#'   \item{Freq}{no idea}
#'   \item{hit.count}{total number of hits distributed through canopy}
#'   \item{phi}{percent of saturation}
#'   \item{dee}{percent of returns distributed}
#'   \item{x.counter}{counting variable}
#'   \item{sum.dee}{distributed proportion}
#'   \item{fee}{coefficent}
#'   \item{cvr}{cover proportion}
#'   \item{olai}{max LAI or VAI number}
#'   \item{vai}{calculated VAI}
#' }
#' @source \url{http://atkinsjeff.github.io}
"pcl_vai"

#' summary matrix
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 40 rows:
#' \describe{
#'   \item{xbin}{x-bin position}
#'   \item{mean.ht}{mean height}
#'   \item{sd.ht}{standard deviation of mean leaf height}
#'   \item{max.ht}{max measured height}
#'   \item{max.vai}{highest measured max VAI}
#'   \item{sum.vai}{total VAI for the column}
#'   \item{sd.vai}{standard deviation of VAI}
#'   \item{vai.z.sum}{density adjuste height}
#'   \item{max.vai.z}{height of peak VAI}
#'   \item{height.bin}{mean leaf height}
#' }
#' @source \url{http://atkinsjeff.github.io}
"pcl_summary"

#' a data frame LiDAR returns that have been split to x and z position
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 13982 rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#'   \item{sky_hit}{lidar return that does not hit the canopy}
#'   \item{can_hit}{lidar return that hits the canopy}
#'   \item{marker}{negative value that indicates marker}
#'   \item{seg_num}{intermediate to get x position}
#'   \item{chunk_num}{intermediate to get x position}
#'   \item{xbin}{position along horizontal axis}
#'   \item{zbin}{position along vertical axis}
#' }
#' @source \url{http://atkinsjeff.github.io}
"pcl_split"

#' a data frame of normalized LiDAR return density
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 1120 rows:
#' \describe{
#'   \item{.id}{column numbering}
#'   \item{xbin}{x-bin position}
#'   \item{zbin}{z-bin position}
#'   \item{bin.hits}{number of LiDAR returns at each x- and z- bin}
#'   \item{sky.hits}{total numer of sky hits per x column}
#'   \item{can.hits}{total numer of canopy hits per x column}
#'   \item{lidar.pulses}{no. of lidar pulses emitted per column}
#'   \item{Freq}{no idea}
#'   \item{hit.count}{total number of hits distributed through canopy}
#'   \item{phi}{percent of saturation}
#'   \item{dee}{percent of returns distributed}
#'   \item{x.counter}{counting variable}
#'   \item{sum.dee}{distributed proportion}
#'   \item{fee}{coefficent}
#' }
#' @source \url{http://atkinsjeff.github.io}
"pcl_norm"

#' a LiDAR hit density by x, z position
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 1120 rows:
#' \describe{
#'   \item{xbin}{x-bin position}
#'   \item{zbin}{z-bin position}
#'   \item{bin.hits}{number of LiDAR returns at each x- and z- bin}
#'   \item{sky.hits}{total numer of sky hits per x column}
#'   \item{can.hits}{total numer of canopy hits per x column}
#'   \item{lidar.pulses}{no. of lidar pulses emitted per column}
#'   \item{Freq}{no idea}
#'   }
#'   @source \url{http://atkinsjeff.github.io}
"pcl_matrix"

#' PCL transect from the University of Virginia
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' #' @format A data frame with 14576rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#' }
#' @source \url{http://atkinsjeff.github.io}
"pcl_data"


#' a data frame LiDAR returns that have been split to x and z position and coded
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 14576 rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#'   \item{sky_hit}{lidar return that does not hit the canopy}
#'   \item{can_hit}{lidar return that hits the canopy}
#'   \item{marker}{negative value that indicates marker}
#'   }
#'   @source \url{http://atkinsjeff.github.io}
"pcl_coded"

#' a data frame LiDAR returns that have been split to x and z position and coded
#'  and adjusted for user height
#'
#' Derived from data collected at the University of Virginia
#' Data collected August, 2016 by J. Atkins. Dervied from the calc_vai function
#'
#' @format A data frame with 14576 rows:
#' \describe{
#'   \item{index}{index of raw data--position along transect}
#'   \item{return_distance}{raw, uncorrected LiDAR return distances from laser}
#'   \item{intensity}{intensity values as recorded by LiDAR system}
#'   \item{sky_hit}{lidar return that does not hit the canopy}
#'   \item{can_hit}{lidar return that hits the canopy}
#'   \item{marker}{negative value that indicates marker}
#'   }
#'   @source \url{http://atkinsjeff.github.io}
"pcl_adjusted"
