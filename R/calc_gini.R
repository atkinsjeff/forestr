#' Gini coefficient
#'
#' \code{calc_gini} calculates gini coefficient
#'
#' The \code{calc_gini} function calculates gini coefficient, a varialbe the describes dissimilarity
#'
#'
#' @param m matrix of light adjusted vai values.
#' @param method "MH" is MacArthur-Horn and "Bohrer" is the Bohrer method
#'
#' @keywords statisitcs
#' @export
#' @return gini coefficient
#' @examples
#'
#' calc_gini(pcl_vai, method = "Bohrer")
#'

calc_gini <- function(m, method) {
  #
  dx <- NULL
  df.z <- NULL
  zmean <- NULL
  area.x <- NULL
  area.null <- NULL
  A <- NULL
  B <- NULL
  gini <- NULL

  if (method == "Bohrer"){

  # A new data frame of VAI distributed at each height level
  df.z <- stats::aggregate(vai ~ zbin, data = m, FUN = sum)

  # Manual Reimann integral
  dx <- diff(df.z$vai)

  zmean <- (df.z$zbin[-1] + df.z$zbin[-length(df.z$zbin)]) / 2

  # Area under the curve
  area.x <- sum(dx * zmean)

  # now for area of the 1:1
  # this approach makes it reflexive
  area.null <- (max(df.z$zbin) * max(df.z$vai)) /  2

  # Make the gini coeff
  A <- area.null - area.x
  B <- area.x

  # Calculate gini
  gini <- A / (A + B)

  } else if (method == "MH"){

    # A new data frame of VAI distributed at each height level
    df.z <- stats::aggregate(lad ~ zbin, data = m, FUN = sum)

    # Manual Reimann integral
    dx <- diff(df.z$lad)

    zmean <- (df.z$zbin[-1] + df.z$zbin[-length(df.z$zbin)]) / 2

    # Area under the curve
    area.x <- sum(dx * zmean)

    # now for area of the 1:1
    # this approach makes it reflexive
    area.null <- (max(df.z$zbin) * max(df.z$lad)) /  2

    # Make the gini coeff
    A <- area.null - area.x
    B <- area.x

    # Calculate gini
    gini <- A / (A + B)

  }

  message("Gini Coefficient")
  print(gini)
  return(gini)
}
