pcl_vai <- calc_vai(pcl_norm, max.vai = 8)

devtools::use_data(pcl_vai, overwrite = TRUE)
