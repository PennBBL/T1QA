# AFGR October 9th 2015

##Usage##
# This script will be used to return skewness and kurtosis of an image
# The requirements are the raw nifti image that would like to be analyzed and a binary mask of the image.
# This script should be called within the bash wrapper. 

# Turn off warnings
options(warn=-1)

##Declare libraries
library("ANTsR")
library("psych")


## Create functions
# Create a function to return the matrix of the image
# This script should be fed the path to the anatomical image, as well as the mask for the ROI
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

# Create a function which will return the skewnes and kurtosis
matrixSkewandKurt <- function(mat){
  # Create the kurtosis and skewness variables
  outputVars <- describeBy(mat)[11:12]
  
  # Now return the variables
  return(outputVars)
}

# Declare our images of interest
anatImage <- commandArgs()[5]
maskImage <- commandArgs()[6]

# Now lets get the values we want
tmp <- img2matrix(anatImage, maskImage)
tmp <- matrixSkewandKurt(tmp)

# Now write these values to STDOUT
write(as.character(tmp), stdout())



