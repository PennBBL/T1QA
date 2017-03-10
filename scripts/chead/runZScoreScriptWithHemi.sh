#!/bin/bash

# This script is going to be used to create the z score images for the qap paper
# It's going to be a really quick and dirty script
# bad codeing practices abound! 

# Lets first make all of our directories
mkdir -p /home/arosen/T1QA/data/ct
mkdir -p /home/arosen/T1QA/data/gmd
mkdir -p /home/arosen/T1QA/data/vol

# Now make our left and right values for our individual modalities 
grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIct.csv > /home/arosen/T1QA/data/ct/rightVals
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIct.csv > /home/arosen/T1QA/data/ct/leftVals

grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIgmd.csv > /home/arosen/T1QA/data/gmd/rightVals
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIgmd.csv > /home/arosen/T1QA/data/gmd/leftVals

grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIvol.csv > /home/arosen/T1QA/data/vol/rightVals
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIvol.csv > /home/arosen/T1QA/data/vol/leftVals

# Now run through each of the previously created files and make our hemisphere mask
cd /home/arosen/T1QA/data/ct/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/ct/rightVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/ct/outputImage.nii.gz /home/arosen/T1QA/data/ct/outputImageRight.nii.gz 
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/ct/leftVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/ct/outputImage.nii.gz /home/arosen/T1QA/data/ct/outputImageLeft.nii.gz 

cd /home/arosen/T1QA/data/gmd/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/gmd/rightVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/gmd/outputImage.nii.gz /home/arosen/T1QA/data/gmd/outputImageRight.nii.gz
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/gmd/leftVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/gmd/outputImage.nii.gz /home/arosen/T1QA/data/gmd/outputImageLeft.nii.gz

cd /home/arosen/T1QA/data/vol/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/vol/rightVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/vol/outputImage.nii.gz /home/arosen/T1QA/data/vol/outputImageRight.nii.gz
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/vol/leftVals 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/vol/outputImage.nii.gz /home/arosen/T1QA/data/vol/outputImageLeft.nii.gz


#####
#####
## Now do all of this for the validation data!!
#####
#####
## Its just going to be a copy and paste 
#####
mkdir -p /home/arosen/T1QA/data/ctValid
mkdir -p /home/arosen/T1QA/data/gmdValid
mkdir -p /home/arosen/T1QA/data/volValid

grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIctValid.csv > /home/arosen/T1QA/data/ctValid/rightValsValid
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIctValid.csv > /home/arosen/T1QA/data/ctValid/leftValsValid

grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIgmdValid.csv > /home/arosen/T1QA/data/gmdValid/rightValsValid
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIgmdValid.csv > /home/arosen/T1QA/data/gmdValid/leftValsValid

grep "_R_" /home/arosen/T1QA/data/jlfSigQAPROIvolValid.csv > /home/arosen/T1QA/data/volValid/rightValsValid
grep "_L_" /home/arosen/T1QA/data/jlfSigQAPROIvolValid.csv > /home/arosen/T1QA/data/volValid/leftValsValid

cd /home/arosen/T1QA/data/ctValid/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/ctValid/rightValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/ctValid/outputImage.nii.gz /home/arosen/T1QA/data/ctValid/outputImageRightValid.nii.gz 
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/ctValid/leftValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/ctValid/outputImage.nii.gz /home/arosen/T1QA/data/ctValid/outputImageLeftValid.nii.gz 


cd /home/arosen/T1QA/data/gmdValid/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/gmdValid/rightValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/gmdValid/outputImage.nii.gz /home/arosen/T1QA/data/gmdValid/outputImageRightValid.nii.gz
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/gmdValid/leftValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/gmdValid/outputImage.nii.gz /home/arosen/T1QA/data/gmdValid/outputImageLeftValid.nii.gz

cd /home/arosen/T1QA/data/volValid/
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/volValid/rightValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 44 -uthr 44 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/volValid/outputImage.nii.gz /home/arosen/T1QA/data/volValid/outputImageRightValid.nii.gz
/home/arosen/T1QA/scripts/chead/makeZScoreJLFPNCTemplateImage.sh /home/arosen/T1QA/data/volValid/leftValsValid 4
fslmaths /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/jlf/pncTemplateJLF_Labels.nii.gz -thr 45 -uthr 45 -bin -mul 1616 -add outputImage.nii.gz outputImage.nii.gz
mv /home/arosen/T1QA/data/volValid/outputImage.nii.gz /home/arosen/T1QA/data/volValid/outputImageLeftValid.nii.gz
