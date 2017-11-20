# AFGR October 24 2016
# This script is going to be used to compute the effect size of the outcome of the 1 vs 2 model on individual ROI values
# cortical thickness and volume values will be being predicted as well as GMD just for the heck of it.
# This will be done in both hemispheres which is also important to note.
# The various steps involved in this process are:
#	0.) Produce age regressed values : may or may not include this
#	1.) Model our individual ROI following this method: lm(individualROI ~ qapValues)
#	2.) Perform FDR correction on all ROI's


## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
tbvData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_antsCtVol.csv')
## Now load all of the freesurfer values
fsVol <- read.csv('/home/adrose/qapQA/data/n1601_freesurferVol_20161220.csv')
fsCt <- read.csv('/home/adrose/qapQA/data/n1601_freesurferCt_20161220.csv')
fsVals <- merge(fsVol, fsCt, by=c('bblid', 'scanid'))
fsQAVals <- read.csv('~/n1601_t1QaData_20170306.csv')
## Source all functions
source('/home/adrose/T1QA/scripts/galton/computeROIWiseEffectSizeFucntions.R')

## Load library(s) we will need
install_load('caret', 'lme4', 'bda', 'ggplot2', 'R.matlab')

## Now lets prep the data
## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- merge(raw.lme.data, tbvData, by='bblid')
raw.lme.data <- merge(raw.lme.data, fsQAVals)
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
#raw.lme.data[,3:33] <- scale(raw.lme.data[,3:33], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]
## Now create our outcomes
# First create our zero vs not zero outcome for everyone
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
validationData$variable <- rep('ratingNULL', nrow(validationData))
# Now lets do our 1 vs 2 model for everyone
trainingData$oneVsTwoOutcome <- trainingData$mean_euler#predict(oneVsTwoModel, newdata=trainingData,
#allow.new.levels=T, type='response')
validationData$oneVsTwoOutcome <- validationData$mean_euler#predict(oneVsTwoModel, newdata=validationData,
#allow.new.levels=T, type='response')
## Now merge our scaled data values with the original data values
all.train.data <- merge(mergedQAP, trainingData, by='bblid')
all.train.data <- merge(all.train.data, fsVals, by='bblid')
all.valid.data <- merge(mergedQAP, validationData, by='bblid')
all.valid.data <- merge(all.valid.data, fsVals, by='bblid')

# FIrst I need to produce tables for flagged images for the train and test data set
all.train.data$fsFlag[which(all.train.data$fsReviewExclude!='NA')] <- 1
nCol <- table(all.train.data$averageManualRating)
flagColumn <- table(all.train.data$fsFlag, all.train.data$averageManualRating)
rmColumn <- table(all.train.data$fsReviewExclude, all.train.data$averageManualRating)
outputCol <- rbind(nCol, flagColumn, rmColumn)
rownames(outputCol) <- c('n', 'NotFlagged', 'Flagged', 'Include', 'Exclude')
write.csv(outputCol, 'trainFlagStatus.csv', quote=F)
# Now do the valid data set 
all.valid.data$fsFlag[which(all.valid.data$fsReviewExclude!='NA')] <- 1
nCol <- table(all.valid.data$averageManualRating)
flagColumn <- table(all.valid.data$fsFlag, all.valid.data$averageManualRating)
rmColumn <- table(all.valid.data$fsReviewExclude, all.valid.data$averageManualRating)
outputCol <- rbind(nCol, flagColumn, rmColumn)
rownames(outputCol) <- c('n', 'NotFlagged', 'Flagged', 'Include', 'Exclude')
write.csv(outputCol, 'testFlagStatus.csv', quote=F)

# Remove 0 scans 
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating!=0),]
# Now rm the FS outliers 
all.train.data <- all.train.data[which(all.train.data$fsFinalExclude==0),]
all.valid.data <- all.valid.data[which(all.valid.data$fsFinalExclude==0),]

# Now create our z scores
tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
fsCTVals <- pvalLoop('_thickness', tmp, correct=TRUE)
fsCTVals <- fsCTVals[-grep('ean', fsCTVals[,1]),]
write.csv(fsCTVals, 'ctZScoreTrain.csv')
rm(tmp)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
fsVOLVals <- pvalLoop('_volume', all.train.data, TBV=F, correct=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(fsCTVals[,2], colorScaleNeg=c('blue', 'light blue'),colorScalePos=c('yellow', 'red'))[-1,]
ctColors[,8] <- fsCTVals[,1]
ctColors <- cbind(ctColors, fsCTVals[,2])
volColors <- returnPosNegAndNeuColorScale(fsVOLVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))[-1,]
volColors[,8] <- fsVOLVals[,1]
volColors <- cbind(volColors, fsVOLVals[,2])

# Now I need to save these color scales and the other thing
writeMat('fsctColorScale.mat', vals=ctColors)

# Now do the validation data down here
static <- all.train.data
all.train.data <- all.valid.data

tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
fsCTVals <- pvalLoop('_thickness', tmp, correct=TRUE)
fsCTVals <- fsCTVals[-grep('ean', fsCTVals[,1]),]
write.csv(fsCTVals, 'ctZScoreTest.csv')
rm(tmp)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
fsVOLVals <- pvalLoop('_volume', all.train.data, TBV=F,correct=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(fsCTVals[,2], colorScaleNeg=c('blue', 'light blue'),colorScalePos=c('yellow', 'red'))[-1,]
ctColors[,8] <- fsCTVals[,1]
ctColors <- cbind(ctColors, fsCTVals[,2])
volColors <- returnPosNegAndNeuColorScale(fsVOLVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))[-1,]
volColors[,8] <- fsVOLVals[,1]
volColors <- cbind(volColors, fsVOLVals[,2])

# Now I need to save these color scales and the other thing
writeMat('fsctvalidColorScale.mat', vals=ctColors)
