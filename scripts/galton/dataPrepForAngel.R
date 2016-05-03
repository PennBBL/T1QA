## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm')

## Declare some functions
###################
## Load the data ##
###################
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n1601_qap_output.csv")
#kurtVals <- read.csv("/home/adrose/qapQA/data/allTissueSkewAndKurtVals.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n1601_skew_kurt_values.csv")
manualQAData <- read.csv("/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n1601_manual_ratings.csv")
dateData <- read.csv("/home/adrose/qapQA/data/cohort_list.csv", header=F)


# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
manualQAData2$averageRating[which(manualQAData2$averageRating>.99 & manualQAData2$averageRating< 1.34)] <- 1

manualQAData <- merge(manualQAData, manualQAData2, by='bblid')
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

# Now merge the data 
mergedQAP <- merge(qapRawOutput, manualQAData, by="scanid")
mergedQAP <- mergedQAP[!duplicated(mergedQAP),]


# Fix dateData's colnames
colnames(dateData) <- c('bblid', 'scanid', 'date')
mergedQAP <- merge(mergedQAP, dateData, by='scanid')
mergedQAP <- merge(mergedQAP, manualQAData2, by='bblid')

# Now create a bblid vector
attach(mergedQAP)
outputDF <- as.data.frame(cbind(bblid.y, as.character(scanid), date, ratingJB, ratingKS, ratingLV, averageRating, qi1, bg.kurtosis))
colnames(outputDF)[2] <- 'scanid'
colnames(outputDF)[1] <- 'bblid'
detach(mergedQAP)
write.csv(outputDF, 'qaInformationForAngel.csv', row.names=F, quote=F)
