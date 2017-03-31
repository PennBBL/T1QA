# AFGR June 13 2016
# This script is going to be used to plot the p cor between the 1 vs 2 model and our average imaging metrics 
## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
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
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data#[index,]
validationData <- raw.lme.data#[-index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')

## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
tmp <- cbind(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], all.train.data[,grep('mprage_jlf_vol', names(all.train.data))])
# Now trim non cortical regions
tmp <- tmp[,-c(seq(1, 38), seq(137,172))]
meanCT <- NULL
for(i in seq(1, nrow(tmp))){
  tmpVal <- weighted.mean(x=tmp[i,1:98], w=tmp[i,99:196])
  meanCT <- append(meanCT, tmpVal)
}
tmp <- read.csv('/home/adrose/qapQA/data/averageGMD.csv')
all.train.data <- merge(all.train.data, tmp, by=c('bblid', 'scanid'))
rm(tmp)
all.train.data$meanVOL <- apply(all.train.data[,2628:2725], 1, sum)

# Now create our scaled age and age squared values
all.train.data$age <- scale(all.train.data$ageAtGo1Scan)
all.train.data$ageSq <- (scale(all.train.data$ageAtGo1Scan))^2

# Now produce our age regressed values 
all.train.data$meanCTAgeReg <- lm(meanCT ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanFSCtAgeReg <- rep('NA', nrow(all.train.data))
topindex <- as.numeric(names(lm(bh.meanthickness ~ age + ageSq + sex, data=all.train.data)$residuals))
all.train.data$meanFSCtAgeReg[topindex] <- as.numeric(lm(bh.meanthickness ~ age + ageSq + sex, data=all.train.data)$residuals)
all.train.data$meanFSAreaAgeReg <- as.numeric(lm(bh.totalarea ~ age + ageSq + sex, data=all.train.data)$residuals)
all.train.data$TotalGrayVolAgeReg <- 'NA'
all.train.data$TotalGrayVolAgeReg[topindex] <- as.numeric(residuals(lm(CortexVol ~ age + ageSq + sex, data=all.train.data)))
all.train.data$TotalGrayVolAgeReg <- as.numeric(all.train.data$TotalGrayVolAgeReg)

# Now do the age regressed quality metrics
all.train.data$pcaslAgeReg <- residuals(lm(aslEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$idemoAgeReg <- residuals(lm(idemoEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$nbackAgeReg <- residuals(lm(nbackEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$restAgeReg <- residuals(lm(restEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$oneVsTwoOutcomeAgeReg <- lm(oneVsTwoOutcome ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$averageRatingAgeReg <- lm(averageRating ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$ratingJBAgeReg <- lm(ratingJB.x ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$ratingKSAgeReg <- lm(ratingKS.x ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$ratingLVAgeReg <- lm(ratingLV.x ~ age + ageSq + sex, data=all.train.data)$residuals


# Now split into our train and validation data sets
tmp <- all.train.data
all.train.data <- tmp[index,]
all.valid.data <- tmp[-index,]
rm(tmp)

all.train.data <- all.train.data[which(all.train.data$averageRating.x != 0),]
attach(all.train.data)
# Lets do the training data first
meanValsAgeRegAverageRating <- cbind(cor(meanCTAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, averageRatingAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg), averageRatingAgeReg, method='spearman'),
                                       cor(as.numeric(TotalGrayVolAgeReg), averageRatingAgeReg, method='spearman'))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg), oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(TotalGrayVolAgeReg), oneVsTwoOutcomeAgeReg, method='spearman'))

meanValsAgeRegAverageRatingPVal <- cbind(cor.test(meanCTAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(meanGMDAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(meanVOLAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(meanFSCtAgeReg), averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(TotalGrayVolAgeReg), averageRatingAgeReg, method='spearman')$p.value)

meanValsAgeRegOneVsTwoPVal <- cbind(cor.test(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(meanFSCtAgeReg), oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(TotalGrayVolAgeReg), oneVsTwoOutcomeAgeReg, method='spearman')$p.value)

detach(all.train.data)
# Now run through the validation data cor values
all.valid.data <- all.valid.data[which(all.valid.data$averageRating.x!=0),]
attach(all.valid.data)

meanValsAgeRegAverageRatingValid <- cbind(cor(meanCTAgeReg, averageRatingAgeReg, method='spearman'),
                                          cor(meanGMDAgeReg, averageRatingAgeReg, method='spearman'),
                                          cor(meanVOLAgeReg, averageRatingAgeReg, method='spearman'),
                                          cor(as.numeric(meanFSCtAgeReg), averageRatingAgeReg, method='spearman'),
                                          cor(as.numeric(TotalGrayVolAgeReg), averageRatingAgeReg, method='spearman'))

meanValsAgeRegOneVsTwoValid <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                          cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                          cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                          cor(as.numeric(meanFSCtAgeReg), oneVsTwoOutcomeAgeReg, method='spearman'),
                                          cor(as.numeric(TotalGrayVolAgeReg), oneVsTwoOutcomeAgeReg, method='spearman'))

meanValsAgeRegAverageRatingPValValid <- cbind(cor.test(meanCTAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(meanGMDAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(meanVOLAgeReg, averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(meanFSCtAgeReg), averageRatingAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(TotalGrayVolAgeReg), averageRatingAgeReg, method='spearman')$p.value)

meanValsAgeRegOneVsTwoPValValid <- cbind(cor.test(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(meanFSCtAgeReg), oneVsTwoOutcomeAgeReg, method='spearman')$p.value,
                                       cor.test(as.numeric(TotalGrayVolAgeReg), oneVsTwoOutcomeAgeReg, method='spearman')$p.value)
detach(all.valid.data)

# Now prepare our values to graph
trainData <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegOneVsTwo)
trainDataSig <- rbind(meanValsAgeRegAverageRatingPVal, meanValsAgeRegOneVsTwoPVal)
colnames(trainData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Vol')
colnames(trainDataSig) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Vol')
rownames(trainData) <- c('Average Rating', 'Quantification Model')
rownames(trainDataSig) <- c('Average Rating PVal', 'Quantification Model PVal')
trainData <- melt(trainData)
trainDataSig <- melt(trainDataSig)
trainData$Var3 <- rep('Training', nrow(trainData))
trainDataSig$Var3 <- rep('Training', nrow(trainDataSig))
trainDataSig[3] <- p.adjust(trainDataSig[,3], method='fdr')
# Now add a fourth variable to denote signifiacne to train data
trainData$Var4 <- ''
trainData$Var4[which(trainDataSig$value<.05)] <- '*'
trainData$Var4[which(trainDataSig$value<.01)] <- '**'
trainData$Var4[which(trainDataSig$value<.001)] <- '***'

validData <- rbind(meanValsAgeRegAverageRatingValid, meanValsAgeRegOneVsTwoValid)
validDataSig <- rbind(meanValsAgeRegAverageRatingPValValid, meanValsAgeRegOneVsTwoPValValid)
colnames(validData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Vol')
colnames(validDataSig) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Vol')
rownames(validData) <- c('Average Rating', 'Quantification Model')
rownames(validDataSig) <- c('Average Rating PVal', 'Quantification Model PVal')
validData <- melt(validData)
validDataSig <- melt(validDataSig)
validData$Var3 <- rep('Validation', nrow(validData))
validDataSig$Var3 <- rep('Validation', nrow(validDataSig))
validDataSig[3] <- p.adjust(validDataSig[,3], method='fdr')
# Now add a fourth variable to denote signifiacne to our valid data
validData$Var4 <- ''
validData$Var4[which(validDataSig$value<.05)] <- '*'
validData$Var4[which(validDataSig$value<.01)] <- '**'
validData$Var4[which(validDataSig$value<.001)] <- '***'

allData <- as.data.frame(rbind(trainData, validData))
allData$Var1 <- as.factor(allData$Var1)
allData$Var2 <- as.factor(allData$Var2)
allData$value <- as.numeric(as.character(allData$value))
allData$Var3 <- as.factor(allData$Var3)
allData$Var2 <- factor(allData$Var2, levels=c('ANTs CT','FS CT', 'ANTs Vol', 'FS Vol', 'ANTs GMD'))

# Now graph our data
thing1 <- ggplot(allData, aes(x=Var2, y=value, color=Var2, fill=Var1,label=Var4)) +
  geom_bar(stat='identity', position=position_dodge(), color='black', size=.1) +
  geom_text(aes(y=value+.02), color='black', size=10, angle=90, position=position_dodge(1), vjust="middle") + 
  theme_bw() + 
  theme(legend.position="right") +
  labs(title='', x='Imaging Measure', y='Association Between Quality Metric and Imaging Measure') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=20), 
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size=20),
        strip.text.x = element_text(size = 20)) +
  facet_grid(. ~ Var3, space="free_x") +
  guides(fill = guide_legend(title = "Quality Metric", title.theme = element_text(size=20, angle=0)))

png('figure7-partialCorBtn1vs2andAvgRating-withFS.png', width=18, height=12, units='in', res=300)
thing1
dev.off()

## Now comapre individual raters down here
attach(all.train.data)
# Lets do the training data first
meanValsAgeRegJBRating <- cbind(cor(meanCTAgeReg, ratingJBAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingJBAgeReg , method='spearman'),
                                       cor(meanVOLAgeReg, ratingJBAgeReg , method='spearman'))

meanValsAgeRegKSRating <- cbind(cor(meanCTAgeReg, ratingKSAgeReg , method='spearman'),
                                       cor(meanGMDAgeReg, ratingKSAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingKSAgeReg , method='spearman'))

meanValsAgeRegLVRating <- cbind(cor(meanCTAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingLVAgeReg, method='spearman'))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'))
detach(all.train.data)


attach(all.valid.data)
# Now valid data
meanValsAgeRegJBRatingValid <- cbind(cor(meanCTAgeReg, ratingJBAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingJBAgeReg , method='spearman'),
                                       cor(meanVOLAgeReg, ratingJBAgeReg , method='spearman'))

meanValsAgeRegKSRatingValid <- cbind(cor(meanCTAgeReg, ratingKSAgeReg , method='spearman'),
                                       cor(meanGMDAgeReg, ratingKSAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingKSAgeReg , method='spearman'))

meanValsAgeRegLVRatingValid <- cbind(cor(meanCTAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, ratingLVAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, ratingLVAgeReg, method='spearman'))

meanValsAgeRegOneVsTwoValid <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'))
detach(all.valid.data)

# Now prepare our values to graph
trainData <- rbind(meanValsAgeRegJBRating, meanValsAgeRegKSRating, meanValsAgeRegLVRating, meanValsAgeRegOneVsTwo)
colnames(trainData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol')
rownames(trainData) <- c('p < 0.05', 'p < 0.05', 'p < 0.05', 'p < 0.05')
trainData <- melt(trainData)
trainData$Var3 <- rep('Training', nrow(trainData))

validData <- rbind(meanValsAgeRegJBRatingValid, meanValsAgeRegKSRatingValid, meanValsAgeRegLVRatingValid, meanValsAgeRegOneVsTwoValid)
colnames(validData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol')
rownames(validData) <- c('p < 0.05', 'p < 0.05', 'p < 0.05', 'p < 0.05')
validData <- melt(validData)
validData$Var3 <- rep('Validation', nrow(validData))

allData <- as.data.frame(rbind(trainData, validData))
allData$Var1 <- as.factor(allData$Var1)
allData$Var2 <- as.factor(allData$Var2)
allData$value <- as.numeric(as.character(allData$value))
allData$Var3 <- as.factor(allData$Var3)
allData$Var2 <- factor(allData$Var2, levels=c('ANTs CT','ANTs Vol', 'ANTs GMD'))

thing1 <- ggplot(allData, aes(x=Var2, y=value, color=Var2, fill=Var1, group=Var1)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1, colour="black") +
  theme_bw() + 
  theme(legend.position="none") +
  labs(title='', x='Imaging Measure', y='Association Between Quality Metric and Imaging Measure') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=20), 
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text = element_text(size=20),
        text=element_text(size=20),
        axis.text.y = element_text(size=30)) +
  facet_grid(. ~ Var3, space="free_x") +
  guides(fill = guide_legend(title = "Quality Measure"))

png('supp-partialCorBtn1vs2andIndividualRating.png', width=18, height=12, units='in', res=300)
thing1
dev.off()
