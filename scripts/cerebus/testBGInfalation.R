# AFGR March 1st 2016

##Usage##
# This script is going to be used to add noise to the BG of images
# That have been run through the qap pipeline
# The basic workflow will follow what can be found below
#   1.) Load Anatomical image
#   2.) Load BG mask
#   3.) Find raw Images BG rate of decay for the distribution
#   4.) modify the rate of decay and create the new bg values
#   5.) Add the new BG values to the raw image
#   6.) Write the image output

# Turn off warnings
options(warn=-1)

##Declare libraries
library("ANTsR")

## Create functions

# Create a function which will output the log liklihood of a distribution
f <- function(x,theta){
  -sum(dexp(x,rate=theta,log=T))
}


# Declare our images of interest
anatImage <- commandArgs()[5]
maskImage <- commandArgs()[6]
outputName <- commandArgs()[7]

####
# 1.) Load the anatomical image
####
anatImage <- antsImageRead(anatImage)

####
# 2.) Load the mask image
####
maskImage <- antsImageRead(maskImage)
logMask <- (maskImage == 1)

####
# 2.1.) Find the raw bg
####
anatBGImage <- anatImage[logMask]

####
# 3.) Now find the raw image's bg image distribution
####
inputTheta <- optimize(f=f, x = anatBGImage , interval=seq(0.01,1,.01))$minimum

####
# 4.) Now modify the rate of decay
####
newBGImage <- round(rexp(length(anatBGImage), rate = (inputTheta * .5)),digit=0)

####
# 5.) Change the BG values with the new ones
####
anatImage[logMask] <- newBGImage

####
# 6.) Now write the new output image & write the new theta value out to stdout
####
antsImageWrite(anatImage, filename = outputName)
write(as.character(inputTheta * .5), stdout())



