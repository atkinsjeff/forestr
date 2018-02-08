pcl_summary <- make_summary_matrix(pcl_split, pcl_vai)

devtools::use_data(pcl_summary, overwrite = TRUE)
