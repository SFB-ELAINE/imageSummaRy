# imageSummaRy
R package to summarize (pixel) information of a tiff-image

# Use the package
```R
# Testscript V.1
rm(list = ls())
graphics.off()


list.of.packages <- c("tiff", "dplyr", "stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(devtools)


#document()
#check()
#build()

load_all()

## FIRST EXAMPLE DIRECTORY -------------------------------------------------

# Parameter values
#input_file <- "inst/image2.tif"
#output_directory <- "inst/"
input_directory <- "inst/"



# Execution of main function
df_results <- imageSummaRy(input_directory = input_directory)

```