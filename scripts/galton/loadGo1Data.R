## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'plyr', 'visreg', 'scales', 'stats', 'lme4','reshape2')

## Declare some functions


## Load the data
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n1601_qap_output.csv")
#kurtVals <- read.csv("/home/adrose/qapQA/data/allTissueSkewAndKurtVals.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n1601_skew_kurt_values.csv")
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n1601_manual_ratings.csv")
allFSData <- read.csv("/home/adrose/qapQA/data/bilateral.meanthickness.totalarea.csv")
allJLFData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_jlfVol.csv')
allJLFDataGMD <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_jlfGMD.csv')
allJLFDataCT <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_jlfCt.csv')
allJLFData <- merge(allJLFData, allJLFDataGMD, by=c('bblid', 'scanid'))
allJLFData <- merge(allJLFData, allJLFDataCT, by=c('bblid', 'scanid'))

# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
attach(manualQAData2)
rawAverageRating <- as.data.frame(cbind(bblid, averageRating))
colnames(rawAverageRating)[2] <- 'rawAverageRating'
detach(manualQAData2)
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
#manualQAData2$averageRating[which(manualQAData2$averageRating>.99)] <- 1
manualQAData2 <- merge(manualQAData2, rawAverageRating, by='bblid')

manualQAData <- merge(manualQAData, manualQAData2, by='bblid')
qapRawOutput <- merge(qapRawOutput,kurtVals ,by.x="subject", by.y="bblid")

# add bblid and scan id columns to qapRawOutput variable 
# prime a NA value bblid column

qapRawOutput$bblid <- strSplitMatrixReturn(qapRawOutput$subject, '_')[,1]
qapRawOutput$scanid <- strSplitMatrixReturn(qapRawOutput$subject, '_')[,2]
allFSData$scanid <- strSplitMatrixReturn(allFSData$scanid, 'x')[,2]

# Now go through each subject id and split the string and reutnr just the first value of that strsplit
#for (subjectIndex in 1:length(qapRawOutput$subject)){
#  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][1]
#  qapRawOutput$bblid[subjectIndex] <- stringSplitOutput
#  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][2]
#  qapRawOutput$scanid[subjectIndex] <- stringSplitOutput
#}

# Now turn them back into factors 
qapRawOutput$bblid <- as.factor(qapRawOutput$bblid)
qapRawOutput$scanid <- as.factor(qapRawOutput$scanid)

# Now merge the data 
mergedQAP <- merge(qapRawOutput, manualQAData, by=c('bblid', 'scanid'))
mergedQAP <- merge(mergedQAP, allFSData, by=c('bblid', 'scanid'))
mergedQAP$bh.meanthickness <- apply(mergedQAP[,c(2520, 2522)], 1, function(x) mean(x, na.rm=T))
mergedQAP$bh.totalarea <- apply(mergedQAP[,c(2521, 2523)], 1, function(x) sum(x, na.rm=T))
mergedQAP <- merge(mergedQAP, allJLFData, by=c('bblid', 'scanid'))

## Declare some variables
manualQAValue <- "averageRating"

manualQAColVal <- grep(manualQAValue, names(mergedQAP))

qapValNames <- names(mergedQAP)[6:41]

## Now prep some derivative data sets
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
goodvsBadData <- isolatedVars[which(isolatedVars[manualQAValue]==0),]
goodvsBadData <- rbind(goodvsBadData, isolatedVars[which(isolatedVars[manualQAValue]==2),])
goodvsBadData$averageRating[which(goodvsBadData$averageRating==2)] <- 1
goodvsBadData$averageRating <- factor(goodvsBadData$averageRating)

## Now I want to run the lmer for rating(0 vs !0) ~ (1|rater) + rating + first 12 pca compontents
## The input data will have 16?? columns these include:
## 1.) bblid 2.) rating 3.) rater 4-16.) Compontnets from PCA
## First rm the size variables from the isolated variables df
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
