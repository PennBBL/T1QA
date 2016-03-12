#!/bin/bash

###############
## AFGR March 1 2016
## This script is going to be used to find all of the voxel counts and
## % of voxels greater then an arbitray cut off for bg images
## That have been run through the QAP pipeline
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
  echo "findPercent -i <anatImage> -m <bgMask> -c <cutOff>"
  echo "Anatomical image required"
  echo "Mask image is required"
  echo
  echo
  echo "*** This script will only run on host: cerebus.uphs.upenn.edu ***"
  echo
  exit 1
}


## Read the inputs 
while getopts "i:m:c:h" OPTION ; do
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
    c)
      cutOff=$OPTARG
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
percentVal=`R --slave -f /home/adrose/skewAndKurtTest/scripts/percentGreaterThen22.R ${anatImage} ${maskImage}  ${cutOff}`
percentVal=`echo ${percentVal} | rev | cut -f 1-2 -d ' ' | rev`

## Now print the output
echo "Count Percent"
echo ${percentVal}

## Check to see if we created an image
if [ ${tmpCreateFlag} -eq 1 ] ; then
  rm ${maskImage} ; 
fi
