# AFGR June 13 2016
# This script is going to be used to produce the p cords between our motion metrics and the outcome imaging metrics
# it's going to be used for all of the motino metrics just liek figure 12 in the QAP project 
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
all.train.data$meanCT <- apply(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], 1, mean)
tmp <- read.csv('/home/adrose/qapQA/data/averageGMD.csv')
all.train.data <- merge(all.train.data, tmp, by=c('bblid', 'scanid'))
rm(tmp)
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, sum)

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

# Now do the age regressed quality metrics
all.train.data$pcaslAgeReg <- residuals(lm(aslEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$idemoAgeReg <- residuals(lm(idemoEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$nbackAgeReg <- residuals(lm(nbackEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$restAgeReg <- residuals(lm(restEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$oneVsTwoOutcomeAgeReg <- lm(oneVsTwoOutcome ~ age + ageSq + sex, data=all.train.data)$residuals


# Now split into our train and validation data sets
tmp <- all.train.data
all.train.data <- tmp[index,]
all.valid.data <- tmp[-index,]
rm(tmp)

all.train.data <- all.train.data[which(all.train.data$averageRating.x != 0),]
attach(all.train.data)
# Lets do the training data first
meanValsAgeRegPcaslT <- cbind(cor(meanCTAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, pcaslAgeReg, use='complete', method='spearman'))

meanValsAgeRegIdemoT <- cbind(cor(meanCTAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, idemoAgeReg, use='complete', method='spearman'))

meanValsAgeRegNbackT <- cbind(cor(meanCTAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, nbackAgeReg, use='complete', method='spearman'))

meanValsAgeRegRestT <- cbind(cor(meanCTAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, restAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwoT <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))


detach(all.train.data)

# Now do the valid data
all.valid.data <- all.valid.data[which(all.valid.data$averageRating.x!=0),]
attach(all.valid.data)

# Now produce the cor values
meanValsAgeRegPcaslV <- cbind(cor(meanCTAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, pcaslAgeReg, use='complete', method='spearman'))

meanValsAgeRegIdemoV <- cbind(cor(meanCTAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, idemoAgeReg, use='complete', method='spearman'))

meanValsAgeRegNbackV <- cbind(cor(meanCTAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, nbackAgeReg, use='complete', method='spearman'))

meanValsAgeRegRestV <- cbind(cor(meanCTAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, restAgeReg, use='complete', method='spearman'))
meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwoV <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))


detach(all.valid.data)


# Now prepare our values to graph
trainData <- rbind(meanValsAgeRegPcaslT, meanValsAgeRegIdemoT, meanValsAgeRegNbackT, meanValsAgeRegRestT, meanValsAgeRegOneVsTwoT)
colnames(trainData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(trainData) <- c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', '1 vs 2 Model')
trainData <- melt(trainData)
trainData$Var3 <- rep('Training', nrow(trainData))

validData <- rbind(meanValsAgeRegPcaslV, meanValsAgeRegIdemoV, meanValsAgeRegNbackV, meanValsAgeRegRestV, meanValsAgeRegOneVsTwoV)
colnames(validData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(validData) <- c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', '1 vs 2 Model')
validData <- melt(validData)
validData$Var3 <- rep('Validation', nrow(validData))



# Now combine all of our data
allData <- as.data.frame(rbind(trainData, validData))
allData$Var1 <- factor(allData$Var1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', '1 vs 2 Model'))
allData$Var2 <- factor(allData$Var2, levels=c('FS CT', 'ANTs CT', 'FS Vol', 'ANTs Vol', 'ANTs GMD', 'FS Area'))
allData$value <- abs(as.numeric(as.character(allData$value)))
allData$Var3 <- as.factor(allData$Var3)
allData <- allData[-grep('FS', allData$Var2),]

# Now plot it 
thing1 <- ggplot(allData, aes(x=Var2, y=value, color=Var2, fill=Var1, group=Var1)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1, colour="black") +
  theme(legend.position="right") +
  labs(title='', x='Structural Imaging Metric', y='Correlation Between \nQuality Measure and Imaging Metric') +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90,hjust=1, size=28), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=28),
        legend.text = element_text(size=20)) +
  facet_grid(. ~ Var3, space="free_x") +
  guides(fill = guide_legend(title = "Quality Measure"))

png('figure17-corvalsOutSideSequence.png', width=20, height=14, units='in', res=300)
thing1
dev.off()
