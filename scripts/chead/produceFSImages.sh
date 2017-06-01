#!/bin/bash

# AFGR May 31st 2017
# This script will be used to produce the FS images requireed for the QAP paper

# Declare some statics
matlabScript="/home/arosen/T1QA/scripts/chead/produceFSImages.m"
dirHome="/home/arosen/T1QA/data"
declare -a arr=("fsct" "fsvol" "fsctvalid" "fsvolvalid")

# Make the directories 
for i in "${arr[@]}" ; do 
  mkdir -p ${dirHome}/${i} ;
done

# Now run the matlab script for each image
for i in "${arr[@]}" ; do 
  echo ${i}
  # Now run the matlab script 
  matlab -nodisplay -r "produceFSImages('/home/arosen/T1QA/data/${i}ColorScale.mat'); exit;" 
  mv *png ${dirHome}/${i}/
done
