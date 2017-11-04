#' Portable Canopy LiDAR from Ordway-Swisher Biological Station
#'
#' Data collected in 2016 during an EAGER-NEON funded field campaign.
#'
#' @docType data
#'
#' @usage data(osbs)
#'
#' @format A three-column .csv file with an index, return_distance(m), and intensity from PCL
#'
#' @keywords datasets
#'
#' @references Atkins, JW, RT Fahey, BH Hardiman, CM Gough (In Review) Forest structural complexity
#'  predicts canopy light absorption at sub-continental scales. In review at JGR-Biogeosciences
#'
#' @references Atkins, Jeff (2017): Canopy Structural Complexity Metrics from Terrestrial LiDAR
#'  and Light Data from 11 Eastern US Forests. figshare.
#'
#' @source \href{https://doi.org/10.6084/m9.figshare.5540485.v1} {Figshare}
#'
#' @examples
#' data(osbs)
#' head(osbs)
#' \donttest{
#' process_pcl(osbs, marker.spacing = 10, user_height = 1)
