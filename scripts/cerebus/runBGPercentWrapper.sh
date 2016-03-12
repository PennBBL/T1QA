#/bin/bash

#AFGR October 14 2015

# This bash script is going to be use to produce a csv with bblid_scanid and kurtosis and skewness measures for all input #subjects provided by a hard lined txt file


# Declare some inputs
skewScript="/home/adrose/skewAndKurtTest/scripts/runBgPercents.sh"
subjectList="/home/adrose/skewAndKurtTest/data/mprageFilePath${1}.txt"
outputBase="/import/GO/incoming_eons_all_bg_inflation/subjects/"
outputCSV="/home/adrose/skewAndKurtTest/reports/percentVals${1}.csv"

# Create file header
echo "bblid,kurtosis,skewness" > ${outputCSV}

# Create a for loop and run through each subject through the skew value script
for i in `cat ${subjectList}` ; do 
  randDig=`echo ${RANDOM}`
  bblid=`echo ${i} | cut -f 6 -d '/'`
  maskImg=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/qap_head_mask/*`
  #outputDir="${outputBase}${bblid}/mprage/nifti/"
  #mkdir -p ${outputDir}
  #fslmaths ${maskImg} -mul -1 -add 1 /home/adrose/temp_${randDig}
  #maskImg="/home/adrose/temp_${randDig}.nii.gz"
  allVals=`${skewScript} -i ${i} -m ${maskImg} -c 50`
  numVox=`echo ${allVals} | cut -f 3 -d ' '`
  percentNum=`echo ${allVals} | cut -f 4 -d ' '`
  echo ${bblid} ${numVox} ${percentNum} >> ${outputCSV}  ; 
done
