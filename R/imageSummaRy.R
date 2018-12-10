#' @title imageSummaRy
#' @description Main function for image summary
#' @details Input should be tif-format. The output is the summary in the
#' data frame format.
#' @aliases imagesummary imageSummery ImageSummary
#' @author Kai Budde
#' @export imageSummaRy
#' @param input_file A character (image file)
#' @param input_directory A character (directory of all images)
#' @param output_directory A character (directory where the resulting images
#' are to be saved)
#' @param threshold A number (minimum intensity for counting pixles)
#' @examples
#' \dontrun{
#' # Obtain all positions of cilia in every z-layer
#' df_image_information <- imageSummaRy(input_files = "inst/image.tif")
#' }
#' 

imageSummaRy <- function(input_file = NULL, input_directory = NULL,
                         output_directory = NULL,
                         threshold = 0) {
  
  # Basics and sourcing functions ------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))
  
  options(stringsAsFactors = FALSE, warn=-1)
  
  # Data input -------------------------------------------------------------
  
  # Input directory must be submitted. If not: close function call.
  if(is.null(input_file) & is.null(input_directory)){
    print(paste("Please call the function with an input file or ",
                "input directory.",
                sep=""))
    return()
  }
  
  # Declare output directory if not given
  if(is.null(output_directory)){
    if(!is.null(input_directory)){
      output_directory <- input_directory
    }else{
      output_directory <- gsub("[[:alnum:]]+\\.tif", "", input_file)
    }
  }
    
  
  # ---------------------------------------------------------------------- #
  # ---------------------- Image Statistics ------------------------------ #
  # ---------------------------------------------------------------------- #
  
  # Create an empty data frame
  df_image_summary <- data.frame(image_name = character(),
                                 dim_x = integer(),
                                 dim_y = integer(),
                                 total_pixel_count = integer(),
                                 red_pixel_count = integer(),
                                 red_pixel_count_percentage = double(),
                                 red_intensity_mean = double(),
                                 red_intensity_sd = double(),
                                 red_intensity_sum = double(),
                                 red_intensity_max = double(),
                                 green_pixel_count = integer(),
                                 green_pixel_count_percentage = double(),
                                 green_intensity_mean = double(),
                                 green_intensity_sd = double(),
                                 green_intensity_sum = double(),
                                 green_intensity_max = double(),
                                 blue_pixel_count = integer(),
                                 blue_pixel_count_percentage = double(),
                                 blue_intensity_mean = double(),
                                 blue_intensity_sd = double(),
                                 blue_intensity_sum = double(),
                                 blue_intensity_max = double(),
                                 stringsAsFactors=FALSE)
  
  if(is.null(input_directory)){
    input_files <- input_file
  }else{
    input_files <- grep("\\.tif", list.files(input_directory), value = TRUE)
  }
  
  
  # Go through all images in directory (or use the given image) ############
  for(i in 1:length(input_files)){
    file_name <- input_files[i]
    file_name <- gsub("\\.tif", "", file_name)
    file_name <- gsub(".*/", "", file_name)
    
    
    # Read in image and save every color layer
    image <- tiff::readTIFF(source = paste(
      input_directory, input_files[i], sep=""), info = FALSE)
    
    #delete 4th layer if there is one
    if(dim(image)[3] == 4){
      image <- drop(image[,,1:3])
    }
    
    
    # Initialization of arrays
    image_red   <- array(data = 0, dim = dim(image))
    image_green <- array(data = 0, dim = dim(image))
    image_blue  <- array(data = 0, dim = dim(image))
    
    image_red_white   <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    image_green_white <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    image_blue_white  <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    
    image_red_grey   <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    image_green_grey <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    image_blue_grey  <- array(data = 0, dim = c(dim(image)[1], dim(image)[2], 1))
    
    # Save information of image
    image_red[,,1]   <- image[,,1]
    image_green[,,2] <- image[,,2]
    image_blue[,,3]  <- image[,,3]
    
    image_red_white[!image[,,1] == 0]   <- 1
    image_green_white[!image[,,2] == 0] <- 1
    image_blue_white[!image[,,3] == 0]  <- 1
    
    
    image_red_grey   <- image[,,1]
    image_green_grey <- image[,,2]
    image_blue_grey  <- image[,,3]
    
    # Calculate statistics and save it in a data frame
    image_name <- file_name
    dim_x <- dim(image)[2]
    dim_y <- dim(image)[1]
    total_pixel_count <- dim_x * dim_y
    red_pixel_count <- sum(image_red != 0)
    red_pixel_count_percentage <- red_pixel_count / total_pixel_count * 100
    red_intensity_mean <- mean(image_red[image_red != 0])
    red_intensity_sd <- stats::sd(image_red[image_red != 0])
    red_intensity_sum <- sum(image_red)
    red_intensity_max <- max(image_red)
    green_pixel_count <- sum(image_green != 0)
    green_pixel_count_percentage <- green_pixel_count / total_pixel_count * 100
    green_intensity_mean <- mean(image_green[image_green != 0])
    green_intensity_sd <- stats::sd(image_green[image_green != 0])
    green_intensity_sum <- sum(image_green)
    green_intensity_max <- max(image_green)
    blue_pixel_count <- sum(image_blue != 0)
    blue_pixel_count_percentage <- blue_pixel_count / total_pixel_count * 100
    blue_intensity_mean <- mean(image_blue[image_blue != 0])
    blue_intensity_sd <- stats::sd(image_blue[image_blue != 0])
    blue_intensity_sum <- sum(image_blue)
    blue_intensity_max <- max(image_blue)
    
    # Save all values in data frame
    row_index <- length(df_image_summary[,1])+1
    df_image_summary[row_index,1] <- image_name
    df_image_summary[row_index,2] <- dim_x
    df_image_summary[row_index,3] <- dim_y
    df_image_summary[row_index,4] <- total_pixel_count
    df_image_summary[row_index,5] <- red_pixel_count
    df_image_summary[row_index,6] <- red_pixel_count_percentage
    df_image_summary[row_index,7] <- red_intensity_mean
    df_image_summary[row_index,8] <- red_intensity_sd
    df_image_summary[row_index,9] <- red_intensity_sum
    df_image_summary[row_index,10] <- red_intensity_max
    df_image_summary[row_index,11] <- green_pixel_count
    df_image_summary[row_index,12] <- green_pixel_count_percentage
    df_image_summary[row_index,13] <- green_intensity_mean
    df_image_summary[row_index,14] <- green_intensity_sd
    df_image_summary[row_index,15] <- green_intensity_sum
    df_image_summary[row_index,16] <- green_intensity_max
    df_image_summary[row_index,17] <- blue_pixel_count
    df_image_summary[row_index,18] <- blue_pixel_count_percentage
    df_image_summary[row_index,19] <- blue_intensity_mean
    df_image_summary[row_index,20] <- blue_intensity_sd
    df_image_summary[row_index,21] <- blue_intensity_sum
    df_image_summary[row_index,22] <- blue_intensity_max
    
    
    # Save the output ########################################################
    # Make sure that the directory contains a "/" at the end
    if(!grepl("/$", output_directory)){
      output_directory <- paste(output_directory, "/", sep="")
    }
    
    tiff::writeTIFF(what = image_red, where = paste(
      output_directory, file_name, "_red.tif", sep=""))
    tiff::writeTIFF(what = image_green, where = paste(
      output_directory, file_name, "_green.tif", sep=""))
    tiff::writeTIFF(what = image_blue, where = paste(
      output_directory, file_name, "_blue.tif", sep=""))
    
    tiff::writeTIFF(what = image_red_white, where = paste(
      output_directory, file_name, "_red_white.tif", sep=""))
    tiff::writeTIFF(what = image_green_white, where = paste(
      output_directory, file_name, "_green_white.tif", sep=""))
    tiff::writeTIFF(what = image_blue_white, where = paste(
      output_directory, file_name, "_blue_white.tif", sep=""))
    
    tiff::writeTIFF(what = image_red_grey, where = paste(
      output_directory, file_name, "_red_grey.tif", sep=""))
    tiff::writeTIFF(what = image_green_grey, where = paste(
      output_directory, file_name, "_green_grey.tif", sep=""))
    tiff::writeTIFF(what = image_blue_grey, where = paste(
      output_directory, file_name, "_blue_grey.tif", sep=""))
  }
  
  return(df_image_summary)

}
