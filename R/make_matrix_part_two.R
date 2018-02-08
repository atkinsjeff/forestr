#' Make PCL matrix part two
#'
#' \code{make_matrix_part_two} produces a matrix of, x, z values in
#' coordinate space with the number and type of each LiDAR
#' return in each x, z bin combination
#'
#'
#' @param df data frame of PCL data that has been processed with
#'
#' @keywords matrix
#' @return sorted matrix of LiDAR returns for each x, z position
#'
#'
#'


make_matrix_part_two <- function(df) {
  #ultimately this should actually make an empty data frame or something
  p <- df

  df2 <- expand.grid(xbin = c(1:max((p$xbin))),
                     zbin = c(0:max((p$zbin))))

  #
  q <- merge(p, data.frame(table(df2[1:2])), all.y=TRUE)
  #now to add empty rows as NA
  #q <- merge(p, data.frame(table(p[1:2]))[-c(3:9)],all.y=TRUE)
  replace(q, is.na(q), 0)#This will correct for any gaps w/out mesmts as all NAs will be 0

}

