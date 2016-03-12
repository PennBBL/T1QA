library(e1071)


# Begin by loading the data
qapRawOutput <- read.csv("/home/adrose/qapQA/data/qap_anatomical_spatial_.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/kurtAndSkewVals.txt", sep=' ')
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
qapRawOutput <- merge(qapRawOutput,kurtVals ,by.x="subject", by.y="bblid")

# add bblid and scan id columns to qapRawOutput variable 
# prime a NA value bblid column
naCol <- rep(NA, length(qapRawOutput$subject))
qapRawOutput$bblid <- naCol
qapRawOutput$scanid <- naCol

# Now go through each subject id and split the string and reutnr just the first value of that strsplit
for (subjectIndex in 1:length(qapRawOutput$subject)){
  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][1]
  qapRawOutput$bblid[subjectIndex] <- stringSplitOutput
  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][2]
  qapRawOutput$scanid[subjectIndex] <- stringSplitOutput
}


# Now turn them back into factors 
qapRawOutput$bblid <- as.factor(qapRawOutput$bblid)
qapRawOutput$scanid <- as.factor(qapRawOutput$scanid)

# Now lets see if we cant merge this data
mergedQAP <- merge(qapRawOutput, manualQAData, by="scanid")
mergedQAP <- mergedQAP[!duplicated(mergedQAP),]

# Isolate our variables of interest into a seperate df
isolatedVars <- mergedQAP[5:30]
foo <- mergedQAP$mprageSbiaExclude
isolatedVars <- cbind(isolatedVars,foo)
isolatedVars$foo <- as.factor(isolatedVars$foo)

# Produce our models
modelAll <- naiveBayes(foo~.,data=isolatedVars)
modelSingle <- naiveBayes(foo~cnr,data=isolatedVars)
modelMulti <- naiveBayes(foo~bg_mean+efc+fwhm+fwhm_x+fwhm_y+fwhm_z+qi1+wm_size, data=isolatedVars)

# Gather output predictions
table(predict(modelAll, isolatedVars))
table(predict(modelSingle, isolatedVars))
table(predict(modelMulti, isolatedVars))
