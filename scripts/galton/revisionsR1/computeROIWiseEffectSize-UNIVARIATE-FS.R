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
# Also load the euler number data
eulerNumber <- read.csv('/home/adrose/qapQA/data/n1601_euler_number.csv')
eulerNumber[,2] <- strSplitMatrixReturn(eulerNumber[,2], 'x')[,2]
eulerNumber$mean_euler <- (eulerNumber[,3] + eulerNumber[,4])/2
eulerNumber[,3:5] <-  scale(eulerNumber[,3:5], scale=T, center=T)
mergedQAP <- merge(mergedQAP, eulerNumber, by=c('bblid', 'scanid'))
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)
tbvData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_antsCtVol.csv')
## Now load all of the freesurfer values
fsVol <- read.csv('/home/adrose/qapQA/data/n1601_freesurferVol_20161220.csv')
fsCt <- read.csv('/home/adrose/qapQA/data/n1601_freesurferCt_20161220.csv')
fsVals <- merge(fsVol, fsCt, by=c('bblid', 'scanid'))
## Source all functions
source('/home/adrose/T1QA/scripts/galton/computeROIWiseEffectSizeFucntions.R')

## Load library(s) we will need
install_load('caret', 'lme4', 'bda', 'ggplot2', 'R.matlab')

## Now lets prep the data
## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- merge(raw.lme.data, tbvData, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]
## Now create our outcomes
# First create our zero vs not zero outcome for everyone
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
validationData$variable <- rep('ratingNULL', nrow(validationData))
# Now lets do our 1 vs 2 model for everyone
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
allow.new.levels=T, type='response')
validationData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=validationData,
allow.new.levels=T, type='response')
## Now merge our scaled data values with the original data values
all.train.data <- merge(mergedQAP, trainingData, by='bblid')
all.train.data <- merge(all.train.data, fsVals, by='bblid')
all.valid.data <- merge(mergedQAP, validationData, by='bblid')
all.valid.data <- merge(all.valid.data, fsVals, by='bblid')

# Remove 0 scans 
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating!=0),]

# Now change our metric of interest
all.train.data$oneVsTwoOutcome <- all.train.data$mean_euler
all.valid.data$oneVsTwoOutcome <- all.valid.data$mean_euler

# Now create our z scores
tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
fsCTVals <- pvalLoop('_thickness', tmp, correct=FALSE)
fsCTVals <- fsCTVals[-grep('ean', fsCTVals[,1]),]
rm(tmp)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
fsVOLVals <- pvalLoop('_volume', all.train.data, TBV=TRUE)
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
writeMat('fsvolColorScale.mat', vals=volColors)

# Now do the validation data down here
static <- all.train.data
all.train.data <- all.valid.data

tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
fsCTVals <- pvalLoop('_thickness', tmp, , correct=FALSE)
fsCTVals <- fsCTVals[-grep('ean', fsCTVals[,1]),]
rm(tmp)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
fsVOLVals <- pvalLoop('_volume', all.train.data, TBV=TRUE)
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
writeMat('fsvolvalidColorScale.mat', vals=volColors)

# Now do MGI
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
mgiEuler <- read.csv('/home/adrose/qapQA/data/mgiEulerVals.csv')
mgiEuler$mean_euler <- (mgiEuler$left_euler + mgiEuler$right_euler)/2
all.train.data <- mergedQAP
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.train.data <- merge(all.train.data, mgiEuler, by='bblid')
all.train.data$oneVsTwoOutcome <- all.train.data$mean_euler
all.train.data$ageAtGo1Scan <- all.train.data$age
all.train.data$sex <- all.train.data$Gender
fsCTVals <- pvalLoop('_thickness', all.train.data, correct=FALSE)
fsCTVals <- fsCTVals[-grep('ean', fsCTVals[,1]),]
## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(fsCTVals[,2], colorScaleNeg=c('blue', 'light blue'),colorScalePos=c('yellow', 'red'))[-1,]
ctColors[,8] <- fsCTVals[,1]
ctColors <- cbind(ctColors, fsCTVals[,2])
# Now I need to save these color scales and the other thing
writeMat('fsctColorScaleMGI.mat', vals=ctColors)
