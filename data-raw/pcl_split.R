pcl_split <- split_transects_from_pcl(pcl_adjusted, transect.length = 40, marker.spacing = 10)

devtools::use_data(pcl_split, overwrite = TRUE)
