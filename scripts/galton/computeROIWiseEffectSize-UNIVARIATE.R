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
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)
tbvData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_antsCtVol.csv')

## Source all functions
source('/home/adrose/T1QA/scripts/galton/computeROIWiseEffectSizeFucntions.R')

## Load library(s) we will need
install_load('caret', 'lme4', 'bda', 'ggplot2')

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
all.valid.data <- merge(mergedQAP, validationData, by='bblid')

# Remove 0 scans 
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating!=0),]


# Now create our z scores
tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
jlfCTVals <- pvalLoop('mprage_jlf_ct', tmp)
rm(tmp)
# RM cortical non cortical regions from gmd values
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2726,2763)]
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
all.train.data <- tmp
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
jlfVOLVals <- pvalLoop('mprage_jlf_vol', all.train.data, TBV=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(jlfCTVals[,2], colorScaleNeg=c('blue', 'light blue'),colorScalePos=c('yellow', 'red'))
gmdColors <- returnPosNegAndNeuColorScale(jlfGMDVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))
volColors <- returnPosNegAndNeuColorScale(jlfVOLVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))

# Now we need to create our label into our file which matches our ROI to our label
jlfCTVals <- cbind(jlfCTVals, ctColors[2:(dim(jlfCTVals)[1]+1),1])
jlfGMDVals <- cbind(jlfGMDVals, gmdColors[2:(dim(jlfGMDVals)[1]+1),1])
jlfVOLVals <- cbind(jlfVOLVals, volColors[2:(dim(jlfVOLVals)[1]+1),1])

# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(volColors, file='volColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIct.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmd.csv', quote=F)
write.csv(jlfVOLVals, 'jlfSigQAPROIvol.csv', quote=F)

# Now do the validation data down here

static <- all.train.data
all.train.data <- all.valid.data

tmp <- all.train.data[,-seq(2862, 2997)[1:38]]
jlfCTVals <- pvalLoop('mprage_jlf_ct', tmp)
rm(tmp)
# RM cortical non cortical regions from gmd values
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2726,2763)]
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
all.train.data <- tmp
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2627,1)]
jlfVOLVals <- pvalLoop('mprage_jlf_vol', all.train.data, TBV=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(jlfCTVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))
gmdColors <- returnPosNegAndNeuColorScale(jlfGMDVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))
volColors <- returnPosNegAndNeuColorScale(jlfVOLVals[,2], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'))

# Now we need to create our label into our file which matches our ROI to our label
jlfCTVals <- cbind(jlfCTVals, ctColors[2:(dim(jlfCTVals)[1]+1),1])
jlfGMDVals <- cbind(jlfGMDVals, gmdColors[2:(dim(jlfGMDVals)[1]+1),1])
jlfVOLVals <- cbind(jlfVOLVals, volColors[2:(dim(jlfVOLVals)[1]+1),1])


# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(volColors, file='volColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIctValid.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmdValid.csv', quote=F)
write.csv(jlfVOLVals, 'jlfSigQAPROIvolValid.csv', quote=F)

