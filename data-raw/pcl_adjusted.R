pcl_adjusted <- adjust_by_user(pcl_coded, user_height = 1.05)

devtools::use_data(pcl_adjusted, overwrite = TRUE)
