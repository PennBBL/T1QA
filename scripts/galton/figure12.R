# AFGR June 13 2016
# This script is going to be used to plot the p cor between the 1 vs 2 model and our average imaging metrics 
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
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

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
all.train.data$cbfMotionAgeReg <- rep('NA', nrow(all.train.data))
cbfIndex <- as.numeric(names(lm(aslEpi10qaMeanrelrms ~ ageAtGo1Scan + sex, data = all.train.data)$residuals))
all.train.data$cbfMotionAgeReg[cbfIndex] <- lm(aslEpi10qaMeanrelrms ~ ageAtGo1Scan + sex, data = all.train.data)$residuals
all.train.data$restMotionAgeReg <- rep('NA', nrow(all.train.data))
restIndex <- as.numeric(names(lm(restEpi10qaMeanrelrms ~ ageAtGo1Scan + sex, data = all.train.data)$residuals))
all.train.data$restMotionAgeReg[restIndex] <- lm(restEpi10qaMeanrelrms ~ ageAtGo1Scan + sex, data = all.train.data)$residuals
all.train.data$restMotionAgeReg <- as.numeric(all.train.data$restMotionAgeReg)
all.train.data$cbfMotionAgeReg <- as.numeric(all.train.data$cbfMotionAgeReg)


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

meanValsAgeRegCbfMotion <- cbind(cor(meanCTAgeReg, cbfMotionAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, cbfMotionAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, cbfMotionAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           cbfMotionAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           cbfMotionAgeReg, method='spearman', use='complete'))

meanValsAgeRegRestMotion <- cbind(cor(meanCTAgeReg, restMotionAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, restMotionAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, restMotionAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           restMotionAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg),
                                           restMotionAgeReg, method='spearman', use='complete'))


outputData <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegZeroVsNotZero, meanValsAgeRegOneVsTwo, 
                    meanValsAgeRegModeRating, meanValsAgeRegRatingJB, meanValsAgeRegRatingLV, meanValsAgeRegRatingKS,
                    meanValsAgeRegCbfMotion, meanValsAgeRegRestMotion)
colnames(outputData) <- c('Ants CT', 'Ants GMD', 'Ants Cerebral Vol', 'FS CT', 'FS Area') 
rownames(outputData) <- c('Avg Rating', '0 vs !0', '1 vs 2', 'Mode Rating', 'JB', 'LV', 'KS', 'CBF', 'REST')
write.csv(outputData, 'manualRegressSpearmanPCor.csv', quote=F)


# Now graph our cors btn the 0 vs !0 model and our average rating
toPlot <- cbind(append(outputData[1,],outputData[2,]), append(rep('AverageRating', length(outputData[1,])), rep('0 vs !0 Model', length(outputData[1,]))), append(colnames(outputData), colnames(outputData)))
rownames(toPlot) <- NULL
toPlot <- as.data.frame(toPlot)
toPlot$V1 <- as.numeric(as.character(toPlot$V1))
toPlot <- toPlot[-c(3, 5, 8, 10),]
pdf('0vsNot0CorValues.pdf', width=18, height=12)
thing1 <- ggplot(toPlot, aes(x=V3, y=V1, color=V2, fill=V2)) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  theme(legend.position="none") +
  labs(title='', x='Structural Imaging Metric', y='Partial Cor Value') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))
thing1
dev.off()

toPlot <- cbind(append(outputData[1,],outputData[3,]), append(rep('AverageRating', length(outputData[1,])), rep('0 vs !0 Model', length(outputData[1,]))), append(colnames(outputData), colnames(outputData)))
rownames(toPlot) <- NULL
toPlot <- as.data.frame(toPlot)
toPlot$V1 <- as.numeric(as.character(toPlot$V1))
toPlot <- toPlot[-c(3, 5, 8, 10),]
pdf('1vs2CorValues.pdf', width=18, height=12)
thing1 <- ggplot(toPlot, aes(x=V3, y=V1, color=V2, fill=V2)) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  theme(legend.position="none") +
  labs(title='', x='Structural Imaging Metric', y='Partial Cor Value') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))
thing1
dev.off()

toPlot <- cbind(c(outputData[1,], outputData[3,], outputData[8,], outputData[9,]), c(rep('AverageRating', length(outputData[1,])), rep('1 vs 2 Model', length(outputData[1,])), rep('CBF Mean Motion', length(outputData[1,])), rep('REST Mean Motion', length(outputData[1,]))), c(colnames(outputData),colnames(outputData),colnames(outputData),colnames(outputData)))
rownames(toPlot) <- NULL
toPlot <- as.data.frame(toPlot)
toPlot$V1 <- as.numeric(as.character(toPlot$V1))
toRM <- grep('Vol', toPlot$V3)
toRM <- append(toRM, grep('Area', toPlot$V3))
toPlot <- toPlot[-toRM,]
pdf('comparingOutsideSequenceVs1vs2.pdf', width=18, height=12)
thing1 <- ggplot(toPlot, aes(x=V3, y=abs(V1), color=V2, fill=V2)) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  theme(legend.position="none") +
  labs(title='', x='Structural Imaging Metric', y='Partial Cor Value') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))
thing1
dev.off()



