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


## Now remove the 0 values because they do weird things
## to our ants values
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating!=0),]
vals <- grep('mprage_jlf_ct', names(all.train.data))
zScoreCT <- NULL
binVals <- NULL
for(i in vals){
    foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
    bar <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[2]
    toAppend <- c(names(all.train.data)[i], foo)
    toAppend2 <- c(names(all.train.data)[i], bar)
    zScoreCT <- rbind(zScoreCT, toAppend)
    binVals <- rbind(binVals, toAppend2)
}
binValsApply <- rep(0, length(vals))
binValsApply[which(p.adjust(binVals[,2], method='fdr')<.05)] <- 1
zScoreCT[,2] <- as.numeric(zScoreCT[,2]) * binValsApply
zScoreCT <- zScoreCT[order(as.numeric(zScoreCT[,2])),]

vals <- grep('mprage_jlf_gmd', names(all.train.data))
# Now rm nonesense ROI's
zScoreGMD <- NULL
binVals <- NULL
for(i in vals[23:120]){
    foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
    toAppend <- c(names(all.train.data)[i], foo)
    zScoreGMD <- rbind(zScoreGMD, toAppend)
}
zScoreGMD <- zScoreGMD[order(as.numeric(zScoreGMD[,2])),]

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(zScoreCT[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
gmdColors <- returnPosNegAndNeuColorScale(zScoreGMD[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))

# Now prepare the key between ROI and intensity
jlfCTVals <- cbind(zScoreCT, seq(1:nrow(zScoreCT)))
jlfGMDVals <- cbind(zScoreGMD, seq(1:nrow(zScoreGMD)))

# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIct.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmd.csv', quote=F)

# Now produce a figure which will display the differences between wioth and without mediation
# I am going to do this for the anterior cingulate cortex - which has the largest positive mediation effect
# To do this I am going to produce the models and then predict the values then model them.
m1 <- lm(mprage_jlf_gmd_R_MFC ~ sex, data = all.train.data)
m2 <- lm(mprage_jlf_gmd_R_MFC ~ sex + oneVsTwoOutcome, data = all.train.data)
m3 <- lm(mprage_jlf_ct_R_ACgG ~ ageAtGo1Scan + averageRating.y, data = all.train.data)
all.train.data$justAge <- residuals(m1)
all.train.data$ageAndQuality <- residuals(m2)

mediationPlot <- ggplot(all.train.data, aes(x=ageAtGo1Scan)) +
  geom_smooth(method=lm, aes(y=justAge), color='red') +
  geom_smooth(method=lm, aes(y=ageAndQuality), color='blue') +
  labs(title='Mediation Effect in TMP CT', x='Age', y='Predicted CT') +
  theme_bw()


# Now do the validation data down here
static <- all.train.data
all.train.data <- all.valid.data

vals <- grep('mprage_jlf_ct', names(all.train.data))
zScoreCT <- NULL
binVals <- NULL
for(i in vals){
    foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
    bar <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[2]
    toAppend <- c(names(all.train.data)[i], foo)
    toAppend2 <- c(names(all.train.data)[i], bar)
    zScoreCT <- rbind(zScoreCT, toAppend)
    binVals <- rbind(binVals, toAppend2)
}
binValsApply <- rep(0, length(vals))
binValsApply[which(p.adjust(binVals[,2], method='fdr')<.05)] <- 1
zScoreCT[,2] <- as.numeric(zScoreCT[,2]) * binValsApply
zScoreCT <- zScoreCT[order(as.numeric(zScoreCT[,2])),]

vals <- grep('mprage_jlf_gmd', names(all.train.data))
# Now rm nonesense ROI's
zScoreGMD <- NULL
binVals <- NULL
for(i in vals[23:120]){
    foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
    toAppend <- c(names(all.train.data)[i], foo)
    zScoreGMD <- rbind(zScoreGMD, toAppend)
}
zScoreGMD <- zScoreGMD[order(as.numeric(zScoreGMD[,2])),]

# Now prepare the key between ROI and intensity
jlfCTVals <- cbind(zScoreCT, seq(1:nrow(zScoreCT)))
jlfGMDVals <- cbind(zScoreGMD, seq(1:nrow(zScoreGMD)))

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(jlfCTVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
gmdColors <- returnPosNegAndNeuColorScale(jlfGMDVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))


# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIctValid.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmdValid.csv', quote=F)

