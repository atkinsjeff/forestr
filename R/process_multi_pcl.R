#' Process multiple PCL transects.
#'
#' \code{process_multi_pcl} imports and processes multiple PCL transect.
#'
#' This is a specific function that works using the input of a data directory of .csv
#' files where the function cycles through the files there and processes multiple
#' files, producing the same output files described in \code{process_pcl}
#'
#' @param data_dir directory where PCL .csv files are stored
#' @param user_height height of laser from ground based on user in meters
#' @param marker.spacing space between markers in the PCL data, in meters
#' @param max.vai the maximum value of column VAI. The default is 8. Should be a max value, not a mean.
#' @param method "MH" is MacArthur-Horn and "Bohrer" is the Bohrer method
#' @param k  correction coeff for MH method (default is 1)
#' @param ht.thresh the height at which to filter values below
#' @param pavd logical input to include Plant Area Volume Density Plot from [plot_pavd], if TRUE it is included, if FALSE, it is not.
#' @param hist logical input to include histogram of VAI with PAVD plot, if TRUE it is included, if FALSE, it is not.
#' @param save_output needs to be set to true, or else you are just going to get a lot of data on the screen
#' @return writes the hit matrix, summary matrix, and output variables to csv in an output folder, along with hit grid plot
#' @keywords file import
#'
#' @export
#'
#' @seealso
#' \code{\link{process_pcl}}
#'
#' @examples
#'
#' # This function works on a directory of raw PCL data
#' \dontrun{data_directory <- "./data/PCL_transects/"  #data directory containing PCL transects
#' process_multi_pcl(data_directory, user_height = 1.05, marker.spacing = 10,
#' max.vai = 8, ht.thresh = 60, pavd = FALSE, h
#' ist = FALSE, save_output = FALSE)
#'
#' process_multi_pcl("./data/PCL_transects/", user_height = 1.05, marker.spacing = 10,
#' max.vai = 8, ht.thresh = 60, pavd = FALSE, hist = FALSE, save_output = FALSE)
#' }
#'
process_multi_pcl <- function(data_dir, user_height = NULL, method = NULL, k = NULL, marker.spacing = NULL, max.vai = NULL, ht.thresh = NULL, pavd = FALSE, hist = FALSE, save_output = TRUE){
  #Global Variables
  output_directory <- NULL

  # If missing user height default is 1 m.
  if(is.null(user_height)){
    user_height = 1
  }

  # If missing user height default is 1 m.
  if(is.null(method)){
    method = "MH"
  }

  # If missing k default is 1 this is the coeff for the MacArthur-Horn
  if(is.null(k)){
    k = 1
  }

  # If missing marker.spacing, default is 10 m.
  if(is.null(marker.spacing)){
    marker.spacing = 10
  }

  # If missing max.vai default is 8
  if(is.null(max.vai)){
    max.vai = 8
  }

  # If missing ht.thresh default is 60
  if(is.null(ht.thresh)){
    ht.thresh = 60
  }

  # If output directory name is missing, add it.
  if(is.null(save_output)){
    save_output == TRUE
    output_dir = "output"
  }

  message("Transect Marker Spacing is:")
  print(marker.spacing)


  file.names <- dir(data_dir, pattern =".CSV", ignore.case = TRUE)


  #for loop that moves through files in directory
  for(i in 1:length(file.names)){
    f <- paste(data_dir, file.names[i], sep = "")
  print(f)
    process_pcl(f, method, user_height, marker.spacing, max.vai, k, ht.thresh, pavd, hist, save_output)
  }

}


  # #begin section of script cribbed from process_pcl
  #     xbin <- NULL
  #     zbin <- NULL
  #     vai <- NULL
  #     key <- NULL
  #     value <- NULL
  #
  #     # If missing user height default is 1 m.
  #     if(missing(user_height)){
  #       user_height = 1
  #     }
  #
  #     # If missing marker.spacing, default is 10 m.
  #     if(missing(marker.spacing)){
  #       marker.spacing = 10
  #     }
  #
  #     # If missing max.vai default is 8
  #     if(missing(max.vai)){
  #       max.vai = 8
  #     }
  #
  #     # If missing ht.thresh default is 60
  #     if(missing(ht.thresh)){
  #       ht.thresh = 60
  #     }
  #
  #     # If output directory name is missing, add it.
  #     if(missing(save_output)){
  #       save_output == TRUE
  #       output_dir = 'output'
  #     }
  #
  #
  #     if(is.character(f) == TRUE) {
  #       # Read in PCL transect.
  #       df <- read_pcl_multi(data_dir, f, ht.thresh)
  #
  #       # Cuts off the directory info to give just the filename.
  #       filename <- sub(".*/", "", f)
  #
  #     } else if(is.data.frame(f) == TRUE){
  #       df <- f
  #       filename <- deparse(substitute(f))
  #     }
  #
  #     # How many NA's in return distance in df?
  #     message("how many in base df have NA")
  #     print(sum(is.na(df$return_distance)))
  #
  #     # Calculate transect length.
  #     transect.length <- get_transect_length(df, marker.spacing)
  #
  #     # cuts out erroneous high values
  #     #df <- df[!(df$return_distance >= 50), ]
  #
  #     # Desginates a LiDAR pulse as either a sky hit or a canopy hit
  #     df <- code_hits(df)
  #
  #     # Adjusts by the height of the  user to account for difference in laser height to ground in meters==default is 1 m.
  #     df <- adjust_by_user(df, user_height)
  #
  #     # Calculate Statistics on Intensity Values
  #     intensity_stats <- calc_intensity(df, filename)
  #
  #     # First-order metrics of sky and cover fraction.
  #     csc.metrics <- csc_metrics(df, filename, transect.length)
  #
  #     # Splits transects from code into segments (distances between markers as designated by marker.spacing
  #     # and chunks (1 m chunks in each marker).
  #     test.data.binned <- split_transects_from_pcl(df, transect.length, marker.spacing)
  #
  #     # creates quantiles from raw data returns...should become it's own function at some point probably
  #     quantiles <- data.frame(stats::quantile(df$return_distance, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
  #     quantiles$key <- as.character(rownames(quantiles))
  #     # remove the percent symbol
  #     quantiles$key <- gsub("[\\%,]", "", quantiles$key)
  #     quantiles$key <- paste0("p", quantiles$key)
  #
  #     names(quantiles)[1] <- "value"
  #     quantiles2 <- tidyr::spread(quantiles, key, value)
  #
  #     print(quantiles)
  #
  #     # Makes matrix of z and x coordinated pcl data.
  #     m1 <- make_matrix(test.data.binned)
  #
  #     # Normalizes date by column based on assumptions of Beer-Lambert Law of light extinction vertically
  #     # through the canopy.
  #     m2 <- normalize_pcl(m1)
  #
  #     # Calculates VAI (vegetation area index m^ 2 m^ -2).
  #     m5 <- calc_vai(m2, max.vai)
  #     m5$.id <- NULL #this removes the weird column I can't tell where it comes from
  #
  #
  #     # Summary matrix.
  #     summary.matrix <- make_summary_matrix(test.data.binned, m5)
  #     rumple <- calc_rumple(summary.matrix)
  #     clumping.index <- calc_gap_fraction(m5)
  #
  #
  #     # foliage height diversity
  #     fhd <- calc_fhd(m5)
  #
  #     # gini coefficient
  #     gini <- calc_gini(m5)
  #
  #     # effective number of layers
  #     enl <- calc_enl(m5)
  #
  #     # combine data and clean data frames
  #     csc.metrics$plot <- NULL
  #     intensity_stats$plot <- NULL
  #
  #     variable.list <- calc_rugosity(summary.matrix, m5, filename)
  #
  #     output.variables <- cbind(variable.list, csc.metrics, rumple,
  #                               clumping.index, enl, fhd, gini, intensity_stats, quantiles2)
  #
  #
  #
  #
  #
  #     vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
  #
  #     #setting up VAI hit grid
  #     m6 <- m5
  #     m6$vai[m6$vai == 0] <- NA
  #     message("No. of NA values in hit matrix")
  #     print(sum(is.na(m6$vai)))
  #     #x11(width = 8, height = 6)
  #     hit.grid <- ggplot2::ggplot(m6, ggplot2::aes(x = xbin, y = zbin))+
  #       ggplot2::geom_tile(ggplot2::aes(fill = vai))+
  #       ggplot2::scale_fill_gradient(low="gray88", high="darkgreen",
  #                                    na.value = "white",
  #                                    limits=c(0, 8),
  #                                    name=vai.label)+
  #       #scale_y_continuous(breaks = seq(0, 20, 5))+
  #       # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
  #       ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
  #                      panel.grid.major = ggplot2::element_blank(),
  #                      panel.grid.minor = ggplot2::element_blank(),
  #                      panel.background = ggplot2::element_blank(),
  #                      axis.text.x = ggplot2::element_text(size = 14),
  #                      axis.text.y = ggplot2::element_text(size = 14),
  #                      axis.title.x = ggplot2::element_text(size = 20),
  #                      axis.title.y = ggplot2::element_text(size = 20))+
  #       ggplot2::xlim(0,transect.length)+
  #       ggplot2::ylim(0,41)+
  #       ggplot2::xlab("Distance along transect (m)")+
  #       ggplot2::ylab("Height above ground (m)")+
  #       ggplot2::ggtitle(filename)+
  #       ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))
  #
  #     if(save_output == TRUE){
  #       output_dir = "output"
  #       #output procedure for variables
  #       dir.create(output_dir, showWarnings = FALSE)
  #       outputname = substr(filename,1,nchar(filename)-4)
  #       outputname <- paste(outputname, "output", sep = "_")
  #       output_directory <- paste("./",output_dir,"/", sep = "")
  #       print(outputname)
  #       print(output_directory)
  #
  #       write_pcl_to_csv(output.variables, outputname, output_directory)
  #       write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
  #       write_hit_matrix_to_csv(m5, outputname, output_directory)
  #
  #
  #
  #
  #       #get filename first
  #       plot.filename <- tools::file_path_sans_ext(filename)
  #       plot.filename.full <- paste(plot.filename, "hit_grid", sep = "_")
  #       plot.filename.pavd <- paste(plot.filename, "pavd", sep = "_")
  #
  #       plot.file.path.hg <- file.path(paste(output_directory, plot.filename.full, ".png", sep = ""))
  #       plot.file.path.pavd <- file.path(paste(output_directory, plot.filename.pavd, ".png", sep = ""))
  #
  #
  #       ggplot2::ggsave(plot.file.path.hg, hit.grid, width = 8, height = 6, units = c("in"))
  #     }
  #
  #
  # }
  # # PAVD
  # if(pavd == TRUE && hist == FALSE && save_output == TRUE){
  #
  #   pavd.plot <- plot_pavd(m5, filename, plot.file.path.pavd)
  #
  # }
  # if(pavd == TRUE && hist == TRUE && save_output == TRUE){
  #
  #   pavd.plot <- plot_pavd(m5, filename, plot.file.path.pavd, hist = TRUE)
  # }
#
# }


  #
