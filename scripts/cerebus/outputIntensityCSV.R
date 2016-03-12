# AFGR December 7 2015

# This script will be used to run a svm on the raw intensity values for al of the images that have been run through 
# the qap pipeline. 
# So the first order is to load all of the images and store the 

# Turn off warnings
options(warn=-1)


## Load library(s)
library("ANTsR")



# Declare some functions here
img2matrix <- function(inputImage, inputMask){
  # Load the images
  loadedAnatImage <- antsImageRead(inputImage)
  loadedMaskImage <- antsImageRead(inputMask)

  # Create a index from the mask
  maskIndex <- (loadedMaskImage==1)

  # Create a matrx with the mask indices
  outputMatrix <- loadedAnatImage[maskIndex]

  # Return the matrix
  return(outputMatrix)
}

# Declare our images of interest
anatImage <- commandArgs()[5]
maskImage <- commandArgs()[6]
csvOutputName <- commandArgs()[7]

# Run the script down here
# Gather a matrix of intensity values here
tmp <- img2matrix(anatImage, maskImage)

# Now vectorize them here
tmp <- c(tmp)

# Now print a csv with these values here
write.csv(tmp, csvOutputName, quote=F, row.names=F)
