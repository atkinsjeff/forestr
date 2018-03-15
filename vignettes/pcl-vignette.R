## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show = 'hold'--------------------------------------------------
require(forestr)
# Link to stored, raw PCL data in .csv form
uva.pcl <- system.file("extdata", "UVAX_A4_01W.csv", package = "forestr")

# Run process complete PCL transect, store output to disk
process_pcl(uva.pcl, marker.spacing = 10, user_height = 1.05, max.vai = 8, pavd = TRUE, hist = TRUE)

