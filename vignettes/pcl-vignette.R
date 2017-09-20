## ---- echo = FALSE, message = FALSE--------------------------------------
library(plyr)
library(dplyr)
library(forestr)

## ---- pcl----------------------------------------------------------------
forestr::process_pcl(osbs, user_height = 1, marker.spacing = 10, max.vai = 8)

