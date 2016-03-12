#/bin/bash

#AFGR October 14 2015

# This bash script is going to be use to produce a csv with bblid_scanid and kurtosis and skewness measures for all input #subjects provided by a hard lined txt file


# Declare some inputs
skewScript="/home/adrose/skewAndKurtTest/scripts/skewAndKurt.sh"
subjectList="/home/adrose/skewAndKurtTest/data/mprageFilePath${1}.txt"
outputCSV="/home/adrose/skewAndKurtTest/reports/BGoutputSkewAndKurtVals${1}.csv"

# Create file header
echo "bblid,kurtosis,skewness" > ${outputCSV}

# Create a for loop and run through each subject through the skew value script
for i in `cat ${subjectList}` ; do 
randDig=`echo ${RANDOM}`
bblid=`echo ${i} | cut -f 6 -d '/'`
maskImg=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/qap_head_mask/*`
maskDir=`ls /import/GO/qapOutputDirectory/test_param/${bblid}/`
mkdir /import/GO/qapOutputDirectory/test_param/${bblid}/*mprage*/nifti/bg_image/
fslmaths ${maskImg} -mul -1 -add 1 -mul /import/GO/incoming_eons_all/subjects/${bblid}/${maskDir}/nifti/mprage.nii.gz /import/GO/qapOutputDirectory/test_param/${bblid}/${maskDir}/nifti/bg_image/${bblid}_bg_image.nii.gz
#maskImg="/home/adrose/temp_${randDig}.nii.gz"
#allVals=`${skewScript} -i ${i} -m ${maskImg}`
#kurtVal=`echo ${allVals} | cut -f 3 -d ' '`
#skewVal=`echo ${allVals} | cut -f 4 -d ' '`
#echo ${bblid} ${kurtVal} ${skewVal} >> ${outputCSV} 
#rm ${maskImg} ; 
done
