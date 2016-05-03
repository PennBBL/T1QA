# AFGR April 26 2016 

# This script is going to be used to explore a couple of more qap analysis options included will be:
#	1.) using a lme to measure IRR both inter and intra???
#	2.) principal compontents analysis on the qap data
#	3.) mixed effects with each rater as a random effect -- not sure how to do this
#	4.) SVM with group weights to avoid classifying way to many bad images


## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'plyr', 'visreg', 'scales', 'stats', 'lme4','reshape2', 'AUC')

## Declare some functions


## Load the data
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n1601_qap_output.csv")
#kurtVals <- read.csv("/home/adrose/qapQA/data/allTissueSkewAndKurtVals.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n1601_skew_kurt_values.csv")
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n1601_manual_ratings.csv")

# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
attach(manualQAData2)
rawAverageRating <- as.data.frame(cbind(bblid, averageRating))
colnames(rawAverageRating)[2] <- 'rawAverageRating'
detach(manualQAData2)
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
manualQAData2$averageRating[which(manualQAData2$averageRating>.99)] <- 1
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

## Declare some variables
manualQAValue <- "averageRating"

manualQAColVal <- grep(manualQAValue, names(mergedQAP))

qapValNames <- names(mergedQAP)[5:40]

## Now prep some derivative data sets
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid.x, isolatedVars)
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
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
components.pca <- prcomp(tmp[,2:37], scale.=T, center=T)
components <- components.pca$x[,1:12]
#lme.data <- melt(tmp, id.vars=c('bblid', 'ratingKS', 'ratingJB', 'ratingLV'),
#                 measure.vars=c('bg_mean','bg_size','bg_std','cnr','csf_mean',
#                 'csf_size','csf_std','efc','fber','fg_mean','fg_size','fg_std',
#                 'fwhm','fwhm_x','fwhm_y','fwhm_z','gm_mean','gm_size','gm_std',
#                 'qi1','snr','wm_mean','wm_size','wm_std','all.kurtosis','all.skewness',
#                 'csf.kurtosis','csf.skewness','gm.kurtosis','gm.skewness','wm.kurtosis',
#                 'bg.skewness','bg.kurtosis'))
tmp <- cbind(tmp, components)

cols.of.interest <- c(1,38,39,40,41,44,45,46,47,48,49,50,51,52,53,54,55)
lme.data <- tmp[,cols.of.interest]
lme.data <- melt(lme.data, id.vars=c('bblid', 'ratingKS', 'ratingJB','ratingLV','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'))
lme.data <- melt(lme.data, id.vars=c('bblid','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'), 
                  measure.vars=c('ratingJB', 'ratingLV', 'ratingKS'))
colnames(lme.data)[2] <- 'binaryOutcome'
lme.data$binaryOutcome <- lme.data$value
lme.data$binaryOutcome[lme.data$binaryOutcome==2] <- 1
lme.data$binaryOutcome <- as.factor(as.numeric(as.character(lme.data$binaryOutcome)))

lmm.1 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data, family='binomial',control=glmerControl(optimizer="bobyqa"))
#sws <- weights(lmm.1)
#lmm.2 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data, family='binomial', weights=sws)

# Produce roc curves
plot(roc(as.vector(predict(lmm.1, type='response')), lme.data$binaryOutcome))

# Validation check
tmp <- as.data.frame(cbind(as.character(mergedQAP$bblid.x), mergedQAP$bg.kurtosis))
vald.data <- merge(lme.data, tmp, by.x = 'bblid', by.y='V1')
lmm.3 <- glmer(binaryOutcome ~ PC3 + (1|variable), family='binomial', data=vald.data)
