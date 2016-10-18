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
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
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
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor')

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

## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
all.train.data$meanCT <- apply(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], 1, mean)
all.train.data$meanGMD <- apply(all.train.data[,grep('mprage_jlf_gmd', names(all.train.data))], 1, mean)
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, mean)
all.train.data$modeRating <- apply(all.train.data[,2978:2980], 1, Mode)



# Now produce our age regressed values 
all.train.data$meanCTAgeReg <- lm(meanCT ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanFSCtAgeReg <- rep('NA', nrow(all.train.data))
topindex <- as.numeric(names(lm(bh.meanthickness ~ ageAtGo1Scan + sex, data=all.train.data)$residuals))
all.train.data$meanFSCtAgeReg[topindex] <- as.numeric(lm(bh.meanthickness ~ ageAtGo1Scan + sex, data=all.train.data)$residuals)
all.train.data$meanFSAreaAgeReg <- as.numeric(lm(bh.totalarea ~ ageAtGo1Scan + sex, data=all.train.data)$residuals)


# Now do the age regressed quality metrics
all.train.data$oneVsTwoOutcomeAgeReg <- lm(oneVsTwoOutcome ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$zeroVsNotZeroOutcomeAgeReg <- lm(zeroVsNotZeroOutcome ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$modeRatingAgeReg <- lm(modeRating ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$averageRatingAgeReg <- lm(rawAverageRating.y ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$ratingJBAgeReg <- lm(ratingJB.y ~ ageAtGo1Scan + sex, data= all.train.data)$residuals
all.train.data$ratingKSAgeReg <- lm(ratingKS.y ~ ageAtGo1Scan + sex, data= all.train.data)$residuals
all.train.data$ratingLVAgeReg <- lm(ratingLV.y ~ ageAtGo1Scan + sex, data= all.train.data)$residuals



all.train.data <- all.train.data[which(all.train.data$averageRating.x != 0),]
attach(all.train.data)

meanValsAgeRegAverageRating <- cbind(cor(meanCTAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           averageRatingAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           averageRatingAgeReg, use='complete', method='spearman'))

meanValsAgeRegZeroVsNotZero <- cbind(cor(meanCTAgeReg, zeroVsNotZeroOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, zeroVsNotZeroOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, zeroVsNotZeroOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           zeroVsNotZeroOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           zeroVsNotZeroOutcomeAgeReg, method='spearman', use='complete'))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'))

meanValsAgeRegModeRating <- cbind(cor(meanCTAgeReg, modeRatingAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, modeRatingAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, modeRatingAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           modeRatingAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           modeRatingAgeReg, method='spearman', use='complete'))

meanValsAgeRegRatingJB <- cbind(cor(meanCTAgeReg, ratingJBAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingJBAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingJBAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingJBAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           ratingJBAgeReg, method='spearman', use='complete'))

meanValsAgeRegRatingLV <- cbind(cor(meanCTAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingLVAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingLVAgeReg, method='spearman', use='complete'))

meanValsAgeRegRatingKS <- cbind(cor(meanCTAgeReg, ratingKSAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingKSAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingKSAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           ratingKSAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           ratingKSAgeReg, method='spearman', use='complete'))

outputData <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegZeroVsNotZero, meanValsAgeRegOneVsTwo, 
                    meanValsAgeRegModeRating, meanValsAgeRegRatingJB, meanValsAgeRegRatingLV, meanValsAgeRegRatingKS)
colnames(outputData) <- c('Ants CT', 'Ants GMD', 'Ants Cerebral Vol', 'FS CT', 'FS Area') 
rownames(outputData) <- c('Avg Rating', '0 vs !0', '1 vs 2', 'Mode Rating', 'JB', 'LV', 'KS')
