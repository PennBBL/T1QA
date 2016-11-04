#!/bin/bash

# This script is going to be used to create the z score images for the qap paper
# It's going to be a really quick and dirty script
# bad codeing practices abound! 

cd /home/arosen/T1QA/scripts/chead
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/toChead/lateralityDocs/negLeft 6
mv /home/arosen/T1QA/scripts/chead/outputImage.nii.gz /home/arosen/T1QA/scripts/chead/outputImageNegLeft.nii.gz

/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/toChead/lateralityDocs/posLeft 6
mv /home/arosen/T1QA/scripts/chead/outputImage.nii.gz /home/arosen/T1QA/scripts/chead/outputImagePosLeft.nii.gz
fslmaths /home/arosen/T1QA/scripts/chead/outputImagePosLeft.nii.gz -add /home/arosen/T1QA/scripts/chead/outputImageNegLeft.nii.gz -mul /data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/100761/20110228x4581/atropos3class_seg_GmMask.nii.gz  bothL
matlab -nodisplay -nojvm -r "returnNumberOfNeighboorhoods('./bothL.nii.gz', 1000); exit;"
mv /home/arosen/T1QA/scripts/chead/thresholdedImage.nii.gz /home/arosen/T1QA/scripts/chead/thresholdedImageL.nii.gz 

/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/toChead/lateralityDocs/negRight 6
mv /home/arosen/T1QA/scripts/chead/outputImage.nii.gz /home/arosen/T1QA/scripts/chead/outputImageNegRight.nii.gz

/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/toChead/lateralityDocs/posRight 6
mv /home/arosen/T1QA/scripts/chead/outputImage.nii.gz /home/arosen/T1QA/scripts/chead/outputImagePosRight.nii.gz
fslmaths /home/arosen/T1QA/scripts/chead/outputImageNegRight.nii.gz -add /home/arosen/T1QA/scripts/chead/outputImagePosRight.nii.gz -mul /data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/100761/20110228x4581/atropos3class_seg_GmMask.nii.gz  bothR
matlab -nodisplay -nojvm -r "returnNumberOfNeighboorhoods('./bothR.nii.gz', 1000); exit;"
mv /home/arosen/T1QA/scripts/chead/thresholdedImage.nii.gz /home/arosen/T1QA/scripts/chead/thresholdedImageR.nii.gz 
