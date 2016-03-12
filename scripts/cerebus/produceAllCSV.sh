#/bin/bash

#AFGR October 14 2015

# This bash script is going to be use to produce a csv with bblid_scanid and kurtosis and skewness measures for all input #subjects provided by a hard lined txt file


# Declare some inputs
intensScript="/home/adrose/svmCerebusTest/scripts/wrapoiCSV.sh"
subjectList="/home/adrose/skewAndKurtTest/data/mprageFilePath${1}.txt"
outputCSV="/home/adrose/skewAndKurtTest/reports/x${1}.csv"

# Create file header
echo "bblid,kurtosis,skewness" > ${outputCSV}

# Create a for loop and run through each subject through the skew value script
for i in `cat ${subjectList}` ; do 
  bblid=`echo ${i} | cut -f 6 -d '/'`
  ${intensScript} -i ${i} -s ${bblid}.csv
done
