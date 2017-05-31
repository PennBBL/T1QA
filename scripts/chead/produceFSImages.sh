#!/bin/bash

# AFGR May 31st 2017
# This script will be used to produce the FS images requireed for the QAP paper


# Make the directories 
dirHome="/home/arosen/T1QA/data"
for i in "fsct fsvol fsctvalid fscolvalid" ; do 
  mkdir -p ${dirHome}/${i} ;
done

# Now run the matlab script for each image
