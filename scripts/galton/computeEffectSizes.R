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

# Now work with the data 
## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel','BaylorEdPsych')


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
					       allow.new.levels=T)
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
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
index <- as.numeric(names(lm(all.train.data$mprage_fs_mean_thickness ~ all.train.data$ageAtGo1Scan)$residuals))
all.train.data$meanFSCtAgeReg[index] <- as.numeric(unname(lm(all.train.data$mprage_fs_mean_thickness ~ all.train.data$ageAtGo1Scan)$residuals))


## Okay we have all of our data points
## Now lets compute some effect sizes 

# lets start with just r
# also start with raw data
attach(all.train.data)
meanValsNoAgeRegAverageRating <- cbind(cor(meanCT, rawAverageRating.y),
                                       cor(meanGMD, rawAverageRating.y),
                                       cor(meanVOL, rawAverageRating.y),
                                       cor(mprage_fs_mean_thickness,
                                           rawAverageRating.y, use='complete'))

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

tableNoAgeReg <- rbind(meanValsNoAgeRegAverageRating, 
                 meanValsNoAgeRegZeroVsNotZero, meanValsNoAgeRegOneVsTwo)
colnames(tableNoAgeReg) <- c('Ants Mean CT', 'Ants Mean GMD', 'Ants Mean Vol', 'FS Mean CT')
rownames(tableNoAgeReg) <- c('Average Rating', '0 vs !0', '1 vs 2')

# Now do age regressed data
meanValsAgeRegAverageRating <- cbind(cor(meanCTAgeReg, rawAverageRating.y),
                                       cor(meanGMDAgeReg, rawAverageRating.y),
                                       cor(meanVOL, rawAverageRating.y),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           rawAverageRating.y, use='complete'))

meanValsAgeRegZeroVsNotZero <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcome),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcome, use='complete'))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcome),
                                       cor(meanGMDAgeReg, oneVsTwoOutcome),
                                       cor(meanVOLAgeReg, oneVsTwoOutcome),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcome, use='complete'))

tableAgeReg <- rbind(meanValsAgeRegAverageRating, 
               meanValsAgeRegZeroVsNotZero, meanValsAgeRegOneVsTwo)
colnames(tableAgeReg) <- c('Ants Mean CT', 'Ants Mean GMD', 'Ants Mean Vol', 'FS Mean CT')
rownames(tableAgeReg) <- c('Average Rating', '0 vs !0', '1 vs 2')

## Now lets get some standardized betas 
all.train.data$standardAge <- scale(ageAtGo1Scan)
all.train.data$standardRating <- scale(rawAverageRating.y)
all.train.data$standardZeroVsNotZero <- scale(zeroVsNotZeroOutcome)
all.train.data$standardOneVsTwo <- scale(oneVsTwoOutcome)
all.train.data$standardMeanGMD <- scale(meanGMD)
all.train.data$standardMeanVOL <- scale(meanVOL)
all.train.data$standardMeanCT <- scale(meanCT)

model1 <- aov(standardMeanGMD ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanGMD ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanGMD ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanGMD ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
gmdEtas <- cbind(model1Eta, model2Eta, model3Eta, model4Eta)
colnames(gmdEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')
model1 <- aov(standardMeanCT ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanCT ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanCT ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanCT ~ standardAge, data=all.train.data)
ctEtas <- cbind(model1Eta, model2Eta, model3Eta, model4Eta)
colnames(ctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')

model1 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(mprage_fs_mean_thickness ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(mprage_fs_mean_thickness ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(mprage_fs_mean_thickness ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
fsctEtas <- cbind(model1Eta, model2Eta, model3Eta, model4Eta)
colnames(fsctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')

model1 <- aov(standardMeanVOL ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanVOL ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanVOL ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanVOL ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[1,2]
model2Eta <- EtaSq(model2)[1,2]
model3Eta <- EtaSq(model3)[1,2]
model4Eta <- EtaSq(model4)[1,2]
volEtas <- cbind(model1Eta, model2Eta, model3Eta, model4Eta)
colnames(volEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')

ageStandBetas <- as.data.frame(rbind(gmdEtas, ctEtas, fsctEtas, volEtas))
colnames(ageStandBetas) <- c('averageRating', '0 vs !0', '1 vs 2', 'No Regression')
rownames(ageStandBetas) <- c('GMD', 'CT', 'FSCT', 'VOL')

# Now I need to compute the 'r' between age and our variable when we regress with our data quality metrics


model1 <- aov(standardMeanGMD ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanGMD ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanGMD ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanGMD ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
gmdEtas <- cbind(model1Eta, model2Eta, model3Eta)

colnames(gmdEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')
model1 <- aov(standardMeanCT ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanCT ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanCT ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanCT ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
ctEtas <- cbind(model1Eta, model2Eta, model3Eta)
colnames(ctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')

model1 <- aov(mprage_fs_mean_thickness ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(mprage_fs_mean_thickness ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(mprage_fs_mean_thickness ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(mprage_fs_mean_thickness ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
fsctEtas <- cbind(model1Eta, model2Eta, model3Eta)
colnames(fsctEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')


model1 <- aov(standardMeanVOL ~ standardAge + standardRating, data=all.train.data)
model2 <- aov(standardMeanVOL ~ standardAge + standardZeroVsNotZero, data=all.train.data)
model3 <- aov(standardMeanVOL ~ standardAge + standardOneVsTwo, data=all.train.data)
model4 <- aov(standardMeanVOL ~ standardAge, data=all.train.data)
model1Eta <- EtaSq(model1)[2,2]
model2Eta <- EtaSq(model2)[2,2]
model3Eta <- EtaSq(model3)[2,2]
volEtas <- cbind(model1Eta, model2Eta, model3Eta)
colnames(volEtas) <- c('averageRating', '0 vs !0', '1 vs 2', 'N/A')

ageStandBetas <- as.data.frame(rbind(gmdEtas, ctEtas, fsctEtas,volEtas))
colnames(ageStandBetas) <- c('averageRating', '0 vs !0', '1 vs 2')
rownames(ageStandBetas) <- c('GMD', 'CT', 'FSCT', 'VOL')

