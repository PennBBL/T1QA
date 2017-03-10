
outputfile="/home/arosen/meanGMDVals.csv"
for subj in `cat /home/arosen/tempCohortListSplit/cohort_list_all_newdates.csv` ; do 
bblid=`echo ${subj} | cut -f 1 -d ,`
scanid=`echo ${subj} | cut -f 2 -d ,`
dateid=`echo ${subj} | cut -f 3 -d ,`
antsPath="/data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/${bblid}/${dateid}x${scanid}/atropos3class_prob02_IsolatedGM.nii.gz"
parcImg=`ls /data/joy/BBL/studies/pnc/processedData/structural/jlf/${bblid}/${dateid}x${scanid}/${bblid}_${dateid}x${scanid}_jlfLabels.nii.gz`
tmpVal=${RANDOM}
tmpImg="/home/arosen/${tmpVal}.nii.gz"
fslmaths ${parcImg} -bin ${tmpImg}
val=`fslstats ${antsPath} -k ${tmpImg} -M`
echo "${bblid},${scanid},${val}" >> ${outputfile} 
rm -f ${tmpImg}; 
done
