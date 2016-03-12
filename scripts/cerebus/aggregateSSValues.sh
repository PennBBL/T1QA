#/bin/bash

#AFGR October 14 2015

# This bash script is going to be use to produce a csv with bblid_scanid and kurtosis and skewness measures for all input #subjects provided by a hard lined txt file


# Declare some inputs
skewScript="/home/adrose/skewAndKurtTest/scripts/kmeansSSTest.sh"
subjectList="/home/adrose/skewAndKurtTest/data/mprageFilePath${1}.txt"
outputCSV="/home/adrose/skewAndKurtTest/reports/SS${1}.csv"

# Create file header
echo "bblid,SS" > ${outputCSV}

# Create a for loop and run through each subject through the skew value script
for i in `cat ${subjectList}` ; do 
  bblid=`echo ${i} | cut -f 6 -d '/'`
  maskImg1=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/anatomical_wm_mask/segment_seg_2.nii.gz`
  maskImg2=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/anatomical_gm_mask/segment_seg_1.nii.gz`
  maskImg3=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/anatomical_csf_mask/segment_seg_0.nii.gz`
  fslmaths ${maskImg1} -add ${maskImg2} -add ${maskImg3} foo.nii.gz
  maskImg="./foo.nii.gz"
  allVals=`${skewScript} -i ${i} -m ${maskImg}`
  kurtVal=`echo ${allVals} | cut -f 3 -d ' '`
  skewVal=`echo ${allVals} | cut -f 4 -d ' '`
  echo ${bblid} ${kurtVal} ${skewVal} >> ${outputCSV} 
  rm -f foo.nii.gz; 
done
