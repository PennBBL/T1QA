# AFGR March 8th 2016

##Usage##
# This script is going to be used to find
# The percentage of the bg voxels
# That are greater then 3 X the average bg intensity 
# for the whole qap dataset
# It will return two things. the number of voxels and the 
# percentage of the total bg voxels that are above 3x bg mean

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
  maskIndex <- (loadedMaskImage==0)

  # Create a matrx with the mask indices
  outputMatrix <- loadedAnatImage[maskIndex]

  # Return the matrix
  return(outputMatrix)
}

# Create a function which will return the skewnes and kurtosis
returnBgCountAndPercent <- function(mat, cutOffPoint){
    # First remove all mat values below the median
    # This is so we are only working with artifacts and not noise
    # This logic was taken from Mortamet 2009 MRM
    mat <- mat[which(mat > median(mat))]
    
    #First create a histogram of the values w/o printing it..duh
    histTmp <- hist(mat, plot=F)
    
    # Now we need to grab the % for each bin
    binPercents <- histTmp$density / sum(histTmp$density)
    
    # Now find percent greater then cut off point
    outputValue <- sum(binPercents[which(histTmp$breaks > cutOffPoint)], na.rm=T)
    
    # Now find # of voxels greater then cutoff value
    voxelCount <- length(which(mat > cutOffPoint))
    
    # Attach the count to the output
    outputValue <- append(voxelCount, outputValue)
    
    # return the values
    return(outputValue)    
}

# Declare our images of interest
anatImage <- commandArgs()[5]
maskImage <- commandArgs()[6]
cutOff <- commandArgs()[7]

# Now lets get the values we want
tmp <- img2matrix(anatImage, maskImage)
tmp <- returnBgCountAndPercent(tmp, cutOff)

# Now write these values to STDOUT
write(as.character(tmp), stdout())



