## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 5)
library(dplyr)
library(forestr)
knit_print.tbl_df <- function(x, options) {
  knitr::knit_print(trunc_mat(x), options)
}

