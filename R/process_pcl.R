#' Process single PCL transects.
#'
#' \code{process_pcl} imports and processes a single PCL transect.
#'
#' This function imports raw pcl data or existing data frames of pcl data and writes
#' all data and analysis to a series of .csv files in an output directory (output)
#' keeping nothing in the workspace.
#'
#' \code{process_pcl} uses a workflow that cuts the data into 1 meter segments with
#' z and x positions in coordinate space where x refers to distance along the ground
#' and z refers to distance above the ground. Data are normalized based on
#' light extinction assumptions from the Beer-Lambert Law to account for light saturation.
#' Data are then summarized and metrics of canopy structure complexity are calculated.
#'
#' \code{process_pcl} will write multiple output files to disk in an output directory that
#'  \code{process_pcl} creates within the work directing. These files include:
#'
#' 1. an output variables file that contains a list of CSC variables and is
#' written by the subfunction \code{write_pcl_to_csv}
#' 2. a summary matrix, that includes detailed information on each vertical column
#' of LiDAR data written by the subfunction \code{write_summary_matrix_to_csv}
#' 3. a hit matrix, which is a matrix of VAI at each x and z position, written by the
#' subfunction \code{write_hit_matrix_to_pcl}
#' 4. a hit grid, which is a graphical representation of VAI along the x and z coordinate space.
#' 5. optionally, plant area/volume density profiles can be created by including
#' \code{pavd = TRUE} that include an additional histogram with the optional
#' \code{hist = TRUE} in the \code{process_pcl} call.
#'
#'
#' @param f  the name of the filename to input <character> or a data frame <data frame>.
#' @param data.type  describes the type of data that is being feed into \code{process_pcl}
#' the options are "continuous" for data without data markers (i.e. -99999999 but `forestr` is going
#' to read any negative \code{return_distance} as a data marker), and "divided"
#' data with data markers.
#' @param user_height the height of the laser off the ground as mounted on the user
#' in meters. default is 1 m
#' @param marker.spacing distance between markers, defaults is 10 m
#' @param transect.length for 'continuous' data without markers, a total transect length is needed.
#' @param max.vai the maximum value of column VAI. The default is 8. Should be
#' a max value, not a mean.
#' @param method "MH" is MacArthur-Horn and "Bohrer" is the Bohrer method
#' @param k  correction coeff for MH method (default is 1)
#' @param ht.thresh the height at which to filter values below default is 60 m
#' @param pavd logical input to include Plant Area Volume Density Plot from {plot_pavd},
#' if TRUE it is included, if FALSE, it is not.
#' @param hist logical input to include histogram of VAI with PAVD plot, if
#' TRUE it is included, if FALSE, it is not.
#' @param save_output the name of the output folder where to write all the output fields.
#'
#' @return writes the hit matrix, summary matrix, and output variables to csv
#' in an output folder, along with hit grid plot
#'
#' @keywords pcl processing
#' @export
#'
#' @seealso
#' \code{\link{process_multi_pcl}}
#'
#'
#' @examples
#'
#' # Run process complete PCL transect without storing to disk
#' uva.pcl <- system.file("extdata", "UVAX_A4_01W.csv", package = "forestr")
#'
#' process_pcl(uva.pcl, method = "MH", user_height = 1.05,
#' k = 1, marker.spacing = 10, ht.thresh = 60, pavd = FALSE, hist = FALSE, save_output = FALSE)
#'
#' # with data frame
#' process_pcl(osbs, marker.spacing = 10, user_height = 1.05, method = "Bohrer", k = 1,
#' max.vai = 8, ht.thresh = 60, pavd = FALSE, hist = FALSE, save_output = FALSE)
#'
#'


process_pcl <- function(f, method = NULL, data.type = NULL, user_height = NULL, k = NULL, transect.length = NULL, marker.spacing = NULL, max.vai = NULL, ht.thresh = NULL, pavd = FALSE, hist = FALSE, save_output = TRUE){
  xbin <- NULL
  zbin <- NULL
  vai <- NULL
  lad <- NULL
  key <- NULL
  value <- NULL

  #### FILLING IN MISSING VALUES FOR INPUT
  # If missing user height default is 1 m.
  if(is.null(user_height)){
    user_height = 1
  }

  # If missing the default method is MacArthur-Horn
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




  #### reading in pcl data and coding filename
  # This works through if f is a file name or a data frame
  if(is.character(f) == TRUE) {

    # Read in PCL transect.
    df<- read_pcl(f, ht.thresh)


    # Cuts off the directory info to give just the filename.
    filename <- sub(".*/", "", f)

    } else if(is.data.frame(f) == TRUE){
    df <- f
    filename <- deparse(substitute(f))
    } else {
    warning('These are not the data you are looking for')
  }



  #### determing data type, spacing, and transect length
  # If file is missing transect.length.
  if(is.null(transect.length)){

  # Calculate transect length.
  transect.length <- get_transect_length(df, marker.spacing)
  } else {
  message("I am straigh up not havin' a good time bro")
  }

  #error checking transect length
  message("Transect Length")
  print(transect.length)


  # Codes data type to handle the presence or absence of data markers
  if(is.null(data.type)){

    min.x <- min(df$return_distance, na.rm = TRUE)

    if(min.x < -1000){

      # this sets data.type to divided, meaning it has markers in the data set
      data.type = "divided"
    } else {

      # make continuous data
      data.type = "continuous"
    }

  }




  ##############################################
  # Adjusts by the height of the  user to account for difference in laser height to
  # ground in   meters==default is 1 m.
  df <- adjust_by_user(df, user_height)

  # Desginates a LiDAR pulse as either a sky hit or a canopy hit
  df <- code_hits(df)

  message("Table of sky hits")
  print(table(df$sky_hit))

  # Calculate Statistics on Intensity Values
  intensity_stats <- calc_intensity(df, filename)

  # First-order metrics of sky and cover fraction.
  csc.metrics <- csc_metrics(df, filename, transect.length)



  ############################
  # splitting transects to x an z bins based on data type.
  if(data.type == "continuous"){

    # working with index values to make x position
    dx = (transect.length / length(df$index))
    df$x <- df$index * dx

    # Code segment to create zbin and xbin
    df$xbin <- ceiling(df$x)
    df$zbin <- round(df$return_distance)
    df$zbin[df$sky_hit == "TRUE"] <- 0

    # creates binned data
    test.data.binned <- df


  } else if(data.type == "divided"){

    # Splits transects from code into segments (distances between markers as designated by        marker.spacing
    # and chunks (1 m chunks in each marker).
    test.data.binned <- split_transects_from_pcl(df, transect.length, marker.spacing)

  }




  #########################
  # creates quantiles from raw data returns...should become it's own function at some point probably
  quantiles <- data.frame(stats::quantile(df$return_distance, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
  quantiles$key <- as.character(rownames(quantiles))

  # remove the percent symbol
  quantiles$key <- gsub("[\\%,]", "", quantiles$key)
  quantiles$key <- paste0("p", quantiles$key)

  names(quantiles)[1] <- "value"
  quantiles2 <- tidyr::spread(quantiles, key, value)

  print(quantiles)

  # Makes matrix of z and x coordinated pcl data.
  m1 <- make_matrix(test.data.binned)

  # # Normalizes date by column based on assumptions of Beer-Lambert Law of light extinction vertically
  # # through the canopy.
  # m2 <- normalize_pcl(m1)
  #

  ################# This section is where the method matters.
  # Normalizes date by column based on assumptions of Beer-Lambert Law of light extinction vertically
  # through the canopy.
  if(method == "Bohrer"){

    # this follows the Bohrer saturation adjustment and uses max.vai
    m2 <- normalize_pcl(m1)

    # makes calculation directly to VAI
    m5 <- calc_vai(m2, max.vai)
    m5$.id <- NULL

  } else if(method == "MH"){

    # this uses strict MacArthur-Horn method and gives LAD
    m5 <- normalize_pcl_mh(m1, k)
    m5$.id <- NULL
  }


  # Summary matrix.
  summary.matrix <- make_summary_matrix(test.data.binned, m5, method)

  # calculates Rumple
  rumple <- calc_rumple(summary.matrix)

  #calculates clumping index
  gap.variables <- calc_gap_fraction(m5)

  # makes them variables
  variable.list <- calc_rugosity(summary.matrix, m5, filename, method)

  # foliage height diversity
  fhd <- calc_fhd(m5, method)

  # entropy calculation similar to lidR package
  entropy <- fhd * log(csc.metrics$max.ht)

  # gini coefficient
  gini <- calc_gini(m5, method)

  # effective number of layers
  enl <- calc_enl(m5, method)

  # combine data and clean data frames
  csc.metrics$plot <- NULL
  intensity_stats$plot <- NULL

  output.variables <- cbind(variable.list, csc.metrics, rumple, gap.variables, enl, fhd, entropy, gini, intensity_stats, quantiles2)


  if(method == "MH"){
    # label for plot
    lad.label =  expression(paste(LAD~(m^2 ~m^-3)))

    #setting up VAI hit grid
    m6 <- m5
    m6$lad[m6$lad == 0] <- NA
    message("No. of NA values in hit matrix")
    print(sum(is.na(m6$lad)))

    # plotting
    hit.grid <- ggplot2::ggplot(m6, ggplot2::aes(x = xbin, y = zbin))+
      ggplot2::geom_tile(ggplot2::aes(fill = lad))+
      ggplot2::scale_fill_gradient(low="gray88", high="darkgreen",
                                   na.value = "white",
                                   limits=c(0, 8),
                                   name=lad.label)+
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 14),
                     axis.text.y = ggplot2::element_text(size = 14),
                     axis.title.x = ggplot2::element_text(size = 20),
                     axis.title.y = ggplot2::element_text(size = 20))+
      ggplot2::xlim(0, transect.length)+
      ggplot2::ylim(0, max(m6$zbin))+
      ggplot2::xlab("Distance along transect (m)")+
      ggplot2::ylab("Height above ground (m)")+
      ggplot2::ggtitle(filename)+
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

  } else if (method == "Bohrer"){

    # label for plot
    vai.label =  expression(paste(VAI~(m^2 ~m^-2)))

    #setting up VAI hit grid
    m6 <- m5
    m6$vai[m6$vai == 0] <- NA
    message("No. of NA values in hit matrix")
    print(sum(is.na(m6$vai)))
    #x11(width = 8, height = 6)
    hit.grid <- ggplot2::ggplot(m6, ggplot2::aes(x = xbin, y = zbin))+
      ggplot2::geom_tile(ggplot2::aes(fill = vai))+
      ggplot2::scale_fill_gradient(low="gray88", high="darkgreen",
                                   na.value = "white",
                                   limits=c(0, 8),
                                   name=vai.label)+
      #scale_y_continuous(breaks = seq(0, 20, 5))+
      # scale_x_continuous(minor_breaks = seq(0, 40, 1))+
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 14),
                     axis.text.y = ggplot2::element_text(size = 14),
                     axis.title.x = ggplot2::element_text(size = 20),
                     axis.title.y = ggplot2::element_text(size = 20))+
      ggplot2::xlim(0,transect.length)+
      ggplot2::ylim(0, max(zbin))+
      ggplot2::xlab("Distance along transect (m)")+
      ggplot2::ylab("Height above ground (m)")+
      ggplot2::ggtitle(filename)+
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"))

  }

  if(save_output == TRUE){
    output_dir = "output"

    #output procedure for variables
    dir.create(output_dir, showWarnings = FALSE)
    outputname = substr(filename,1,nchar(filename)-4)
    outputname <- paste(outputname, "output", sep = "_")
    output_directory <- paste("./",output_dir,"/", sep = "")
    print(outputname)
    print(output_directory)

    write_pcl_to_csv(output.variables, outputname, output_directory)
    write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
    write_hit_matrix_to_csv(m5, outputname, output_directory)

    #get filename first
    plot.filename <- tools::file_path_sans_ext(filename)
    plot.filename.full <- paste(plot.filename, "hit_grid", sep = "_")
    plot.filename.pavd <- paste(plot.filename, "pavd", sep = "_")

    plot.file.path.hg <- file.path(paste(output_directory, plot.filename.full, ".png", sep = ""))
    plot.file.path.pavd <- file.path(paste(output_directory, plot.filename.pavd, ".png", sep = ""))


    ggplot2::ggsave(plot.file.path.hg, hit.grid, width = 8, height = 6, units = c("in"))
    #ggplot2::ggsave(plot.file.path.pavd, plot.pavd, width = 8, height = 6, units = c("in") )

  }


  if(save_output == TRUE){
    output_dir = "output"

    #output procedure for variables
    dir.create(output_dir, showWarnings = FALSE)
    outputname = substr(filename,1,nchar(filename)-4)
    outputname <- paste(outputname, "output", sep = "_")
    output_directory <- paste("./",output_dir,"/", sep = "")
    print(outputname)
    print(output_directory)

    write_pcl_to_csv(output.variables, outputname, output_directory)
    write_summary_matrix_to_csv(summary.matrix, outputname, output_directory)
    write_hit_matrix_to_csv(m5, outputname, output_directory)

    #get filename first
    plot.filename <- tools::file_path_sans_ext(filename)
    plot.filename.full <- paste(plot.filename, "hit_grid", sep = "_")
    plot.filename.pavd <- paste(plot.filename, "pavd", sep = "_")

    plot.file.path.hg <- file.path(paste(output_directory, plot.filename.full, ".png", sep = ""))
    plot.file.path.pavd <- file.path(paste(output_directory, plot.filename.pavd, ".png", sep = ""))


    ggplot2::ggsave(plot.file.path.hg, hit.grid, width = 8, height = 6, units = c("in"))
    #ggplot2::ggsave(plot.file.path.pavd, plot.pavd, width = 8, height = 6, units = c("in") )

  }

}
