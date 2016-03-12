#!/bin/bash

###############
## AFGR October 13 2015
## This script wil be used to output a skewness and kurtosis value for a image given an image and a mask
## This script requires several dependencies:
##	1.) FSL
##	2.) R
##	3.) psych package for R
##	4.) ANTsR package for R
## 
###############

## Usage function
usage(){
  echo
  echo
  echo
  echo "outputIntenVals.sh -i <anatomicalImage.nii.gz> -m <maskImage.nii.gz> -s <output CSV Name>"
  echo "Anatomical Image is required"
  echo "Mask image is optional"
  echo "If no mask is provided the entire image (including 0 voxels will be used to calculate skew and kurt values"
  echo
  echo
  echo "*** This script will only run on host: cerebus.uphs.upenn.edu ***"
  echo
  exit 1
}


## Read the inputs 
while getopts "i:m:h:s:" OPTION ; do
  case $OPTION in 
    h)
      usage
      ;;
    i)
      anatImage=$OPTARG
      ;;
    m)
      maskImage=$OPTARG
      ;;
    s)
      csvName=$OPTARG
      ;;
    ?)
      usage
      ;;
    esac
done

## Check hostname - script has only been tested on cerebus - 
if [ ! `hostname | cut -f 1 -d .` == "cerebus" ] ; then
  echo
  echo "***"
  echo "This script is being called from the wrong host!!"
  echo "***"
  echo
  usage ; 
fi



## Declare some arguments
math="/usr/share/fsl/5.0/bin/fslmaths"
tmpCreateFlag=0
randomVal=${RANDOM}
## Check to see we have at least an anatomical
if [ -z ${anatImage} ] ; then
  echo
  echo
  echo
  echo "This script requires an anatomical Image!"
  echo 
  echo
  usage ; 
fi

## Find what to do about the mask input
if [ -z ${maskImage} ] ; then
  ${math} ${anatImage} -sub ${anatImage} -add 1 tempMask${randomVal}.nii.gz
  maskImage=tempMask${randomVal}.nii.gz 
  tmpCreateFlag=1;
fi

## Now write the intensity values
R --slave -f /home/adrose/svmCerebusTest/scripts/outputIntensityCSV.R ${anatImage} ${maskImage} ${csvName}

## Check to see if we created an image
if [ ${tmpCreateFlag} -eq 1 ] ; then
  rm ${maskImage} ; 
fi
