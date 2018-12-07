#' @title imageSummaRy
#' @description Main function for image summary
#' @details Input should be tif-format.
#' @aliases imagesummary imageSummery ImageSummary
#' @author Kai Budde
#' @export imageSummaRy
#' @param input_file A character (image file)
#' @param color A character (color of the pixels under consideration)
#' @param threshold A number (minimum intensity for counting pixles)
#' @examples
#' \dontrun{
#' # Obtain all positions of cilia in every z-layer
#' df_image_information <- imageSummaRy(input_files = "inst/image.tif")
#' }
#' 

imageSummaRy <- function(input_file, color = "none", threshold = 0) {
  print("Hello, world!")
}
