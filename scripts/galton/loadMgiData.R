## Load Library(s)
#source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
#install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'plyr', 'visreg', 'scales', 'stats', 'lme4','reshape2')

## Declare some functions
###################
## Load the data ##
###################
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n920_qap_output_validation.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n920_skew_kurt_values_validation.csv")
manualQAData <- read.csv("/home/adrose/qapQA/data/n550_mgi_demo_dx_2013-12-13.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n920_manual_ratings_validation.csv")

# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
attach(manualQAData2)
rawAverageRating <- as.data.frame(cbind(bblid, averageRating))
colnames(rawAverageRating)[2] <- 'rawAverageRating'
detach(manualQAData2)
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
manualQAData2$averageRating[which(manualQAData2$averageRating>.99 & manualQAData2$averageRating< 1.34)] <- 1
manualQAData2 <- merge(manualQAData2, rawAverageRating, by='bblid')

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

# Now create the three data sets - Go2, mgi penn, and mgi pitt
mergedQAP.pitt <- mergedQAP[which(mergedQAP$SiteID==70),]
mergedQAP.go2  <- merge(qapRawOutput, manualQAData2, by="bblid")
mergedQAP.go2 <- mergedQAP.go2[which(mergedQAP.go2$bblid %in% mergedQAP$bblid.x == 'FALSE'),]
mergedQAP.penn <- mergedQAP[which(mergedQAP$SiteID==71),]


## Declare some variables
manualQAValue <- "averageRating"

manualQAColVal <- grep(manualQAValue, names(mergedQAP))

qapValNames <- names(mergedQAP)[3:38]

## Now create a data set which only has good and bad data
mergedQAP <- rbind(mergedQAP.penn, mergedQAP.pitt)
#mergedQAP <- mergedQAP[which(mergedQAP$age < 60),]
colnames(mergedQAP) <- gsub(pattern='.x', replacement = '', x = colnames(mergedQAP), fixed = TRUE)
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
goodvsBadData <- isolatedVars[which(isolatedVars[manualQAValue]==0),]
goodvsBadData <- rbind(goodvsBadData, isolatedVars[which(isolatedVars[manualQAValue]==2),])
goodvsBadData$averageRating[which(goodvsBadData$averageRating==2)] <- 1
goodvsBadData$averageRating <- factor(goodvsBadData$averageRating)

## Now I want to load the PCA results and apply the Go1 loadings 
## To the validation data set
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
