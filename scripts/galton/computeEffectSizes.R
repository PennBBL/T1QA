# AFGR September 8 2016
# This script is going to be used to explore the effect size of the mutliple 
# image quality metrics I am using these include:
#	1.) Raw average rating
#	2.) 1 vs !0 model
#	3.) 1 vs 2 model outcome
# I am going to have to compare the effect size of the outcome of these measures
# there are obviously multiple ways to do this.
# I am how ever going to try to do this using eta squared (essentially corelation)
# also I am going to try to do a cohens d... I think by predicting 
# I am super unsure about how to do this I think I want to do this though 
# lm(CT ~ rating)$residuals 
# Do ^ for each of my model outcomes and then find the difference between groups


## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalData.RData')
zeroVsNotZeroModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)
load('/home/adrose/qapQA/data/go1LmerModel.RData')
zeroVsNotZeroLMERModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModelLMER.RData')
oneVsTwoModelLMER <- mod8
rm(mod8)

# pCor function
pCorFunction <- function(imageVals, regVals, ageVals){
  newVals <- lm(imageVals ~ regVals)$residuals
  cor(newVals, ageVals)
}



# Now work with the data 
## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel','BaylorEdPsych', 'mgcv')


## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
trainingData$zeroVsNotZeroOutcome <- predict(zeroVsNotZeroModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
# Now do the 0 vs !0 lmer
trainingData$zeroVsNotZeroOutcomeLMER <- predict(zeroVsNotZeroLMERModel, newdata=trainingData,
                                               allow.new.levels=T)
# Now do the 1 vs 2 lmer 
trainingData$oneVsTwoOutcomeLMER <- predict(oneVsTwoModelLMER, newdata=trainingData,
                                            allow.new.levels=T)


## Now merge our scaled dtaa values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
all.train.data$meanCT <- apply(all.train.data[,2796:2893], 1, mean)
all.train.data$meanGMD <- apply(all.train.data[,2692:2789], 1, mean)
meanGMDWeighted <- NULL
for(i in 1:nrow(all.train.data)){
  meanGMDVal <- weighted.mean(all.train.data[i,2692:2789], all.train.data[i,2567:2664])
  meanGMDWeighted <- append(meanGMDWeighted, meanGMDVal)
}
all.train.data$meanGMDWeighted <- meanGMDWeighted
all.train.data$meanGMDWeightedAgeReg <- lm(meanGMDWeighted ~ ageAtGo1Scan, data=all.train.data)$residuals
rm(meanGMDWeighted)
all.train.data$meanVOL <- apply(all.train.data[,2567:2664], 1, mean)
all.train.data$meanCTAgeReg <- lm(meanCT ~ ageAtGo1Scan, data=all.train.data)$residuals
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ ageAtGo1Scan, data=all.train.data)$residuals
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ ageAtGo1Scan, data=all.train.data)$residuals
all.train.data$meanFSCtAgeReg <- rep('NA', nrow(all.train.data))
topindex <- as.numeric(names(lm(all.train.data$mprage_fs_mean_thickness ~ all.train.data$ageAtGo1Scan)$residuals))
all.train.data$meanFSCtAgeReg[topindex] <- as.numeric(lm(all.train.data$mprage_fs_mean_thickness ~ all.train.data$ageAtGo1Scan)$residuals)
all.train.data$modeRating <- apply(all.train.data[,2927:2929], 1, Mode)
all.train.data$meanFSArea <- apply(all.train.data[,215:282], 1, function(x) mean(x, na.rm=T))
topindex <-  as.numeric(names(lm(all.train.data$meanFSArea ~ all.train.data$ageAtGo1Scan)$residuals))
all.train.data$meanFSAreaAgeReg <- rep('NA', nrow(all.train.data))
all.train.data$meanFSAreaAgeReg[topindex] <- as.numeric(lm(all.train.data$meanFSArea ~ all.train.data$ageAtGo1Scan)$residuals)


## Okay we have all of our data points
## Now lets compute some effect sizes 

# lets start with just r
# also start with raw data
all.train.data <- all.train.data[which(all.train.data$averageRating.x != 0),]
attach(all.train.data)
meanValsNoAgeRegAverageRating <- cbind(cor(meanCT, rawAverageRating.y),
                                       cor(meanGMD, rawAverageRating.y),
                                       cor(meanVOL, rawAverageRating.y),
                                       cor(mprage_fs_mean_thickness,
                                           rawAverageRating.y, use='complete'))

meanValsNoAgeRegModeRating <- cbind(cor(meanCT, modeRating),
                                       cor(meanGMD, modeRating),
                                       cor(meanVOL, modeRating),
                                       cor(mprage_fs_mean_thickness,
                                           modeRating, use='complete'))

meanValsNoAgeRegRatingJB <- cbind(cor(meanCT, ratingJB.y),
                                       cor(meanGMD, ratingJB.y),
                                       cor(meanVOL, ratingJB.y),
                                       cor(mprage_fs_mean_thickness,
                                           ratingJB.y, use='complete'))

meanValsNoAgeRegRatingLV <- cbind(cor(meanCT, ratingLV.y),
                                       cor(meanGMD, ratingLV.y),
                                       cor(meanVOL, ratingLV.y),
                                       cor(mprage_fs_mean_thickness,
                                           ratingLV.y, use='complete'))

meanValsNoAgeRegRatingKS <- cbind(cor(meanCT, ratingKS.y),
                                       cor(meanGMD, ratingKS.y),
                                       cor(meanVOL, ratingKS.y),
                                       cor(mprage_fs_mean_thickness,
                                           ratingKS.y, use='complete'))

meanValsNoAgeRegZeroVsNotZero <- cbind(cor(meanCT, zeroVsNotZeroOutcome),
                                       cor(meanGMD, zeroVsNotZeroOutcome),
                                       cor(meanVOL, zeroVsNotZeroOutcome),
                                       cor(mprage_fs_mean_thickness,
                                           zeroVsNotZeroOutcome, use='complete'))

meanValsNoAgeRegOneVsTwo <- cbind(cor(meanCT, oneVsTwoOutcome),
                                       cor(meanGMD, oneVsTwoOutcome),
                                       cor(meanVOL, oneVsTwoOutcome),
                                       cor(mprage_fs_mean_thickness,
                                           oneVsTwoOutcome, use='complete'))

tableNoAgeReg <- rbind(meanValsNoAgeRegAverageRating, meanValsNoAgeRegModeRating, 
                 meanValsNoAgeRegRatingJB, meanValsNoAgeRegRatingLV, meanValsNoAgeRegRatingKS,
                 meanValsNoAgeRegZeroVsNotZero, meanValsNoAgeRegOneVsTwo)
colnames(tableNoAgeReg) <- c('Ants Mean CT', 'Ants Mean GMD', 'Ants Mean Vol', 'FS Mean CT')
rownames(tableNoAgeReg) <- c('Average Rating', 'Mode Rating', 
                             'ratingJB', 'ratingLV', 'ratingKS',
                             '0 vs !0', '1 vs 2')
write.csv(tableNoAgeReg, 'corBtnRawImagingMetricsAndQUalityMetrics.csv', quote=F)

# Now do age regressed data
meanValsAgeRegAverageRating <- cbind(cor(meanCTAgeReg, rawAverageRating.y),
                                       cor(meanGMDAgeReg, rawAverageRating.y),
                                       cor(meanVOLAgeReg, rawAverageRating.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           rawAverageRating.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           rawAverageRating.y, use='complete'),
                                       cor(meanGMDWeightedAgeReg,rawAverageRating.y))

meanValsAgeRegZeroVsNotZero <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcome, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           zeroVsNotZeroOutcome, use='complete'),
                                       cor(meanGMDWeightedAgeReg, zeroVsNotZeroOutcome))

meanValsAgeRegZeroVsNotZeroLMER <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcomeLMER, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           zeroVsNotZeroOutcomeLMER, use='complete'),
                                        cor(meanGMDWeightedAgeReg, zeroVsNotZeroLMER))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcome),
                                       cor(meanGMDAgeReg, oneVsTwoOutcome),
                                       cor(meanVOLAgeReg, oneVsTwoOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcome, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcome, use='complete'),
                                       cor(meanGMDWeightedAgeReg, oneVsTwoOutcome))

meanValsAgeRegOneVsTwoLMER <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeLMER , use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeLMER , use='complete'),
                                       cor(meanGMDWeightedAgeReg,oneVsTwoOutcomeLMER))

meanValsAgeRegModeRating <- cbind(cor(meanCTAgeReg, modeRating),
                                       cor(meanGMDAgeReg, modeRating),
                                       cor(meanVOLAgeReg, modeRating),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           modeRating, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           modeRating, use='complete'),
                                       cor(meanGMDWeightedAgeReg, modeRating))

meanValsAgeRegRatingJB <- cbind(cor(meanCTAgeReg, ratingJB.y),
                                       cor(meanGMDAgeReg, ratingJB.y),
                                       cor(meanVOLAgeReg, ratingJB.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingJB.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           ratingJB.y, use='complete'),
                                       cor(meanGMDWeightedAgeReg, ratingJB.y))

meanValsAgeRegRatingLV <- cbind(cor(meanCTAgeReg, ratingLV.y),
                                       cor(meanGMDAgeReg, ratingLV.y),
                                       cor(meanVOLAgeReg, ratingLV.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingLV.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingLV.y, use='complete'),
                                       cor(meanGMDWeightedAgeReg, ratingLV.y))

meanValsAgeRegRatingKS <- cbind(cor(meanCTAgeReg, ratingKS.y),
                                       cor(meanGMDAgeReg, ratingKS.y),
                                       cor(meanVOLAgeReg, ratingKS.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingKS.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingKS.y, use='complete'),
                                       cor(meanGMDWeightedAgeReg, ratingKS.y))

tableAgeReg <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegModeRating, 
                 meanValsAgeRegRatingJB, meanValsAgeRegRatingLV, meanValsAgeRegRatingKS,
                 meanValsAgeRegZeroVsNotZero, meanValsAgeRegOneVsTwoLMER, meanValsAgeRegOneVsTwo, meanValsAgeRegOneVsTwoLMER)
colnames(tableAgeReg) <- c('Ants Mean CT', 'Ants Mean GMD', 'Ants Mean Vol', 'FS Mean CT', 'FS Mean Area', 'Ants Mean Weighted GMD')
rownames(tableAgeReg) <- c('Average Rating', 'Mode Rating', 
                             'rating JB', 'rating LV', 'rating KS',
                             '0 vs !0 GLMER', '1 vs 2 GLMER', '0 vs !0 LMER', '1 vs 2 LMER')
write.csv(tableAgeReg, 'corBtnAgeRegImagingDataAndQualityMetrics.csv', quote=F)

## Now lets get some standardized betas 
all.train.data$standardAge <- scale(ageAtGo1Scan)
all.train.data$standardRating <- scale(rawAverageRating.y)
all.train.data$standardZeroVsNotZero <- scale(zeroVsNotZeroOutcome)
all.train.data$standardOneVsTwo <- scale(oneVsTwoOutcome)
all.train.data$standardMeanGMD <- scale(meanGMD)
all.train.data$standardMeanVOL <- scale(meanVOL)
all.train.data$standardMeanCT <- scale(meanCT)
all.train.data$standardMode <- scale(modeRating)
all.train.data$standardRatingJB <- scale(ratingJB.y)
all.train.data$standardRatingKS <- scale(ratingKS.y)
all.train.data$standardRatingLV <- scale(ratingLV.y)
all.train.data$zeroVsNotZeroLMER <- scale(zeroVsNotZeroOutcomeLMER)
all.train.data$oneVsTwoLMER <- scale(oneVsTwoOutcomeLMER)


model1 <- aov(standardMeanGMD ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanGMD ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanGMD ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanGMD ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanGMD ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanGMD ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanGMD ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanGMD ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
model5Eta <- EtaSq(model5)[1,2]
model6Eta <- EtaSq(model6)[1,2]
model7Eta <- EtaSq(model7)[1,2]
model8Eta <- EtaSq(model8)[1,2]
gmdEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta,model8Eta)
colnames(gmdEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV','N/A')

model1 <- aov(standardMeanCT ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanCT ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanCT ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanCT ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanCT ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanCT ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanCT ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanCT ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
model5Eta <- EtaSq(model5)[1,2]
model6Eta <- EtaSq(model6)[1,2]
model7Eta <- EtaSq(model7)[1,2]
model8Eta <- EtaSq(model8)[1,2]
ctEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta,model8Eta)
colnames(ctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV','N/A')


model1 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(mprage_fs_mean_thickness ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(mprage_fs_mean_thickness ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(mprage_fs_mean_thickness ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(mprage_fs_mean_thickness ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
model5Eta <- EtaSq(model5)[1,2]
model6Eta <- EtaSq(model6)[1,2]
model7Eta <- EtaSq(model7)[1,2]
model8Eta <- EtaSq(model8)[1,2]
fsctEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta,model8Eta)
colnames(fsctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV','N/A')


model1 <- aov(standardMeanVOL ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanVOL ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanVOL ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanVOL ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanVOL ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanVOL ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanVOL ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanVOL ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
model5Eta <- EtaSq(model5)[1,2]
model6Eta <- EtaSq(model6)[1,2]
model7Eta <- EtaSq(model7)[1,2]
model8Eta <- EtaSq(model8)[1,2]
volEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta,model8Eta)
colnames(volEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV','N/A')

ageStandBetas <- as.data.frame(rbind(gmdEtas, ctEtas, fsctEtas, volEtas))
colnames(ageStandBetas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV','N/A')
rownames(ageStandBetas) <- c('GMD', 'CT', 'FSCT', 'VOL')

write.csv(ageStandBetas, 'ageEtas.csv', quote=F)

# Now I need to compute the 'r' between age and our variable when we regress with our data quality metrics
model1 <- lm(standardMeanGMD ~ standardAge + standardRating, data=all.train.data)$fitted.values
model2 <- lm(standardMeanGMD ~ standardAge + standardZeroVsNotZero, data=all.train.data)$fitted.values
model3 <- lm(standardMeanGMD ~ standardAge + standardOneVsTwo, data=all.train.data)$fitted.values
model4 <- lm(standardMeanGMD ~ standardAge + standardMode, data=all.train.data)$fitted.values
model5 <- lm(standardMeanGMD ~ standardAge + standardRatingJB, data=all.train.data)$fitted.values
model6 <- lm(standardMeanGMD ~ standardAge + standardRatingKS, data=all.train.data)$fitted.values
model7 <- lm(standardMeanGMD ~ standardAge + standardRatingLV, data=all.train.data)$fitted.values
model8 <- lm(standardMeanGMD ~ standardAge, data=all.train.data)$fitted.values
model9 <- lm(standardMeanGMD ~ standardAge + zeroVsNotZeroOutcomeLMER, data=all.train.data)$fitted.values
model10 <- lm(standardMeanGMD ~ standardAge + oneVsTwoLMER, data=all.train.data)$fitted.values
model1Eta <- cor(model1, all.train.data$standardMeanGMD)
model2Eta <- cor(model2, all.train.data$standardMeanGMD)
model3Eta <- cor(model3, all.train.data$standardMeanGMD)
model4Eta <- cor(model4, all.train.data$standardMeanGMD)
model5Eta <- cor(model5, all.train.data$standardMeanGMD)
model6Eta <- cor(model6, all.train.data$standardMeanGMD)
model7Eta <- cor(model7, all.train.data$standardMeanGMD)
model8Eta <- cor(model8, all.train.data$standardMeanGMD)
model9Eta <- cor(model9, all.train.data$standardMeanGMD)
model10Eta <- cor(model10, all.train.data$standardMeanGMD)
gmdEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta, model8Eta, model9Eta, model10Eta)
colnames(gmdEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV', 'Age', '0 vs !0 LMER', '1 vs 2 LMER')


model1 <- lm(standardMeanVOL ~ standardAge + standardRating, data=all.train.data)$fitted.values
model2 <- lm(standardMeanVOL ~ standardAge + standardZeroVsNotZero, data=all.train.data)$fitted.values
model3 <- lm(standardMeanVOL ~ standardAge + standardOneVsTwo, data=all.train.data)$fitted.values
model4 <- lm(standardMeanVOL ~ standardAge + standardMode, data=all.train.data)$fitted.values
model5 <- lm(standardMeanVOL ~ standardAge + standardRatingJB, data=all.train.data)$fitted.values
model6 <- lm(standardMeanVOL ~ standardAge + standardRatingKS, data=all.train.data)$fitted.values
model7 <- lm(standardMeanVOL ~ standardAge + standardRatingLV, data=all.train.data)$fitted.values
model8 <- lm(standardMeanVOL ~ standardAge, data=all.train.data)$fitted.values
model9 <- lm(standardMeanVOL ~ standardAge + zeroVsNotZeroOutcomeLMER, data=all.train.data)$fitted.values
model10 <- lm(standardMeanVOL ~ standardAge + oneVsTwoLMER, data=all.train.data)$fitted.values
model1Eta <- cor(model1, all.train.data$standardMeanVOL)
model2Eta <- cor(model2, all.train.data$standardMeanVOL)
model3Eta <- cor(model3, all.train.data$standardMeanVOL)
model4Eta <- cor(model4, all.train.data$standardMeanVOL)
model5Eta <- cor(model5, all.train.data$standardMeanVOL)
model6Eta <- cor(model6, all.train.data$standardMeanVOL)
model7Eta <- cor(model7, all.train.data$standardMeanVOL)
model8Eta <- cor(model8, all.train.data$standardMeanVOL)
model9Eta <- cor(model9, all.train.data$standardMeanVOL)
model10Eta <- cor(model10, all.train.data$standardMeanVOL)
volEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta, model8Eta, model9Eta, model10Eta)
colnames(volEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV', 'Age', '0 vs !0 LMER', '1 vs 2 LMER')

model1 <- lm(standardMeanCT ~ standardAge + standardRating, data=all.train.data)$fitted.values
model2 <- lm(standardMeanCT ~ standardAge + standardZeroVsNotZero, data=all.train.data)$fitted.values
model3 <- lm(standardMeanCT ~ standardAge + standardOneVsTwo, data=all.train.data)$fitted.values
model4 <- lm(standardMeanCT ~ standardAge + standardMode, data=all.train.data)$fitted.values
model5 <- lm(standardMeanCT ~ standardAge + standardRatingJB, data=all.train.data)$fitted.values
model6 <- lm(standardMeanCT ~ standardAge + standardRatingKS, data=all.train.data)$fitted.values
model7 <- lm(standardMeanCT ~ standardAge + standardRatingLV, data=all.train.data)$fitted.values
model8 <- lm(standardMeanCT ~ standardAge, data=all.train.data)$fitted.values
model9 <- lm(standardMeanCT ~ standardAge + zeroVsNotZeroOutcomeLMER, data=all.train.data)$fitted.values
model10 <- lm(standardMeanCT ~ standardAge + oneVsTwoLMER, data=all.train.data)$fitted.values
model1Eta <- cor(model1, all.train.data$standardMeanCT)
model2Eta <- cor(model2, all.train.data$standardMeanCT)
model3Eta <- cor(model3, all.train.data$standardMeanCT)
model4Eta <- cor(model4, all.train.data$standardMeanCT)
model5Eta <- cor(model5, all.train.data$standardMeanCT)
model6Eta <- cor(model6, all.train.data$standardMeanCT)
model7Eta <- cor(model7, all.train.data$standardMeanCT)
model8Eta <- cor(model8, all.train.data$standardMeanCT)
model9Eta <- cor(model9, all.train.data$standardMeanCT)
model10Eta <- cor(model10, all.train.data$standardMeanCT)
ctEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta, model8Eta, model9Eta, model10Eta)
colnames(ctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV', 'Age', '0 vs !0 LMER', '1 vs 2 LMER')

output <- rbind(gmdEtas, volEtas, ctEtas)
write.csv(output, 'agesAbilityToPredictImageModalityWithDiffRegressors.csv', quote=F, row.names=F)

regVals <- c('standardMode',
'standardRatingJB',
'standardRatingKS',
'standardRatingLV',
'zeroVsNotZeroLMER',
'oneVsTwoLMER',
'standardRating',
'standardZeroVsNotZero',
'standardOneVsTwo')

# First lets find the relationship between our values and age
valsToAgeCor <- NULL
for(i in regVals){
    tmp <- as.numeric(unlist(all.train.data[i]))
    corVal <- cor(tmp, all.train.data$ageAtGo1Scan)
    valsToAgeCor <- append(valsToAgeCor, corVal)
}

colnames(valsToAgeCor) <- regVals
write.csv(valsToAgeCor, 'corBtwnDataQualityAndAge.csv', quote=F, row.names=F)
imageVals <- c('standardMeanCT',
'standardMeanVol',
'mprage_fs_mean_thickness',
'standardMeanGMD')

gmdPCorVals <- NULL
for(i in regVals){
  tmp <- as.numeric(unlist(all.train.data[i]))
  corVal <- pCorFunction(all.train.data$standardMeanGMD[,1], tmp, all.train.data$ageAtGo1Scan)
  gmdPCorVals <- append(gmdPCorVals, corVal)
}
gmdPCorVals <- append(gmdPCorVals, cor(all.train.data$standardMeanGMD, all.train.data$ageAtGo1Scan))

gmdWPCorVals <- NULL
for(i in regVals){
    tmp <- as.numeric(unlist(all.train.data[i]))
    corVal <- pCorFunction(all.train.data$meanGMDWeighted, tmp, all.train.data$ageAtGo1Scan)
    gmdWPCorVals <- append(gmdWPCorVals, corVal)
}
gmdWPCorVals <- append(gmdWPCorVals, cor(all.train.data$meanGMDWeighted, all.train.data$ageAtGo1Scan))

ctPCorVals <- NULL
for(i in regVals){
    tmp <- as.numeric(unlist(all.train.data[i]))
    corVal <- pCorFunction(all.train.data$meanCT, tmp, all.train.data$ageAtGo1Scan)
    ctPCorVals <- append(ctPCorVals, corVal)
}
ctPCorVals <- append(ctPCorVals, cor(all.train.data$meanCT, all.train.data$ageAtGo1Scan))

volPCorVals <- NULL
for(i in regVals){
    tmp <- as.numeric(unlist(all.train.data[i]))
    corVal <- pCorFunction(all.train.data$meanVOL, tmp, all.train.data$ageAtGo1Scan)
    volPCorVals <- append(volPCorVals, corVal)
}
volPCorVals <- append(volPCorVals, cor(all.train.data$meanVOL, all.train.data$ageAtGo1Scan))

fsctPCorVals <- NULL
tmp.df <- all.train.data[complete.cases(all.train.data$mprage_fs_mean_thickness),]
for(i in regVals){
    tmp <- as.numeric(unlist(tmp.df[i]))
    corVal <- pCorFunction(tmp.df$mprage_fs_mean_thickness, tmp, tmp.df$ageAtGo1Scan)
    fsctPCorVals <- append(fsctPCorVals, corVal)
}
fsctPCorVals <- append(fsctPCorVals, cor(tmp.df$mprage_fs_mean_thickness, tmp.df$ageAtGo1Scan))
rm(tmp.df)
regVals <- append(regVals, c('No Regressor'))

p.cor.vals <- rbind(gmdPCorVals, gmdWPCorVals, ctPCorVals, volPCorVals, fsctPCorVals)
colnames(p.cor.vals) <- regVals
write.csv(p.cor.vals, 'pCorBtnAgeAndMeanImagingValues.csv', quote=F, row.names=T)
