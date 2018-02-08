
# Import PCL data to the workspace
pcl_data <- read_pcl("UVAX_A4_01W.CSV")


devtools::use_data(pcl_data, overwrite = TRUE)
