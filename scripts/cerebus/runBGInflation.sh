#!/bin/bash

###############
## AFGR March 1 2016
## This script will be used to add aditional noise to an image's background
## although it will work within any area that is included from the input mask
## The goal of this script is to produce images that have inflated BG noise to probe this relationship with qi1
## Necasary dependencies include
##    1.) FSL
##    2.) R
##    3.) ANTsR

###############

## Usage function
usage(){
  echo
  echo
  echo
  echo "addNoise.sh -i <anatImage> -m <bgMask> -o <outputImage>"
  echo "Anatomical image required"
  echo "Mask image is required"
  echo
  echo
  echo "*** This script will only run on host: cerebus.uphs.upenn.edu ***"
  echo
  exit 1
}


## Read the inputs 
while getopts "i:m:o:h" OPTION ; do
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
    o)
      outputImage=$OPTARG
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

## Now produce the skew and kurt values
R --slave -f /home/adrose/artifactInflation/scripts/testBGInfalation.R ${anatImage} ${maskImage}  ${outputImage}


## Check to see if we created an image
if [ ${tmpCreateFlag} -eq 1 ] ; then
  rm ${maskImage} ; 
fi
