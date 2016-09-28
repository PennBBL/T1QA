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
                                           rawAverageRating.y, use='complete'))

meanValsAgeRegZeroVsNotZero <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcome, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           zeroVsNotZeroOutcome, use='complete'))

meanValsAgeRegZeroVsNotZeroLMER <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcomeLMER),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcomeLMER, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           zeroVsNotZeroOutcomeLMER, use='complete'))


meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcome),
                                       cor(meanGMDAgeReg, oneVsTwoOutcome),
                                       cor(meanVOLAgeReg, oneVsTwoOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcome, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcome, use='complete'))

meanValsAgeRegOneVsTwoLMER <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeLMER ),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeLMER , use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeLMER , use='complete'))

meanValsAgeRegModeRating <- cbind(cor(meanCTAgeReg, modeRating),
                                       cor(meanGMDAgeReg, modeRating),
                                       cor(meanVOLAgeReg, modeRating),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           modeRating, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           modeRating, use='complete'))

meanValsAgeRegRatingJB <- cbind(cor(meanCTAgeReg, ratingJB.y),
                                       cor(meanGMDAgeReg, ratingJB.y),
                                       cor(meanVOLAgeReg, ratingJB.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingJB.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           ratingJB.y, use='complete'))

meanValsAgeRegRatingLV <- cbind(cor(meanCTAgeReg, ratingLV.y),
                                       cor(meanGMDAgeReg, ratingLV.y),
                                       cor(meanVOLAgeReg, ratingLV.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingLV.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingLV.y, use='complete'))

meanValsAgeRegRatingKS <- cbind(cor(meanCTAgeReg, ratingKS.y),
                                       cor(meanGMDAgeReg, ratingKS.y),
                                       cor(meanVOLAgeReg, ratingKS.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingKS.y, use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingKS.y, use='complete'))

tableAgeReg <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegModeRating, 
                 meanValsAgeRegRatingJB, meanValsAgeRegRatingLV, meanValsAgeRegRatingKS,
                 meanValsAgeRegZeroVsNotZero, meanValsAgeRegOneVsTwoLMER, meanValsAgeRegOneVsTwo, meanValsAgeRegOneVsTwoLMER)
colnames(tableAgeReg) <- c('Ants Mean CT', 'Ants Mean GMD', 'Ants Mean Vol', 'FS Mean CT', 'FS Mean Area')
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

model1 <- aov(standardMeanGMD ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanGMD ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanGMD ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanGMD ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanGMD ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanGMD ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanGMD ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanGMD ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
model4Eta <- EtaSq(model4)[2,2]
model5Eta <- EtaSq(model5)[2,2]
model6Eta <- EtaSq(model6)[2,2]
model7Eta <- EtaSq(model7)[2,2]
#model8Eta <- EtaSq(model8)[2,2]
gmdEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta)
colnames(gmdEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV')

model1 <- aov(standardMeanCT ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanCT ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanCT ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanCT ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanCT ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanCT ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanCT ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanCT ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
model4Eta <- EtaSq(model4)[2,2]
model5Eta <- EtaSq(model5)[2,2]
model6Eta <- EtaSq(model6)[2,2]
model7Eta <- EtaSq(model7)[2,2]
#model8Eta <- EtaSq(model8)[2,2]
ctEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta)
colnames(ctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV')


model1 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(mprage_fs_mean_thickness ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(mprage_fs_mean_thickness ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(mprage_fs_mean_thickness ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(mprage_fs_mean_thickness ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
model4Eta <- EtaSq(model4)[2,2]
model5Eta <- EtaSq(model5)[2,2]
model6Eta <- EtaSq(model6)[2,2]
model7Eta <- EtaSq(model7)[2,2]
#model8Eta <- EtaSq(model8)[2,2]
fsctEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta)
colnames(fsctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV')


model1 <- aov(standardMeanVOL ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanVOL ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanVOL ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanVOL ~ standardAge + standardMode, data=all.train.data)
model5 <- aov(standardMeanVOL ~ standardAge + standardRatingJB, data=all.train.data)
model6 <- aov(standardMeanVOL ~ standardAge + standardRatingKS, data=all.train.data)
model7 <- aov(standardMeanVOL ~ standardAge + standardRatingLV, data=all.train.data)
model8 <- aov(standardMeanVOL ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
model4Eta <- EtaSq(model4)[2,2]
model5Eta <- EtaSq(model5)[2,2]
model6Eta <- EtaSq(model6)[2,2]
model7Eta <- EtaSq(model7)[2,2]
#model8Eta <- EtaSq(model8)[2,2]
volEtas <- cbind(model1Eta,model2Eta,model3Eta,model4Eta,model5Eta,model6Eta,model7Eta)
colnames(volEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV')

ageStandBetas <- as.data.frame(rbind(gmdEtas, ctEtas, fsctEtas, volEtas))
colnames(ageStandBetas) <- c('averageRating', '0 vs !0', '1 vs 2', 
                      'Mode Rating','JB','KS', 'LV')
rownames(ageStandBetas) <- c('GMD', 'CT', 'FSCT', 'VOL')

write.csv(ageStandBetas, 'qualityMetricsEtas.csv', quote=F)

