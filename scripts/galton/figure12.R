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
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
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
all.train.data$meanGMD <- apply(all.train.data[,grep('mprage_jlf_gmd', names(all.train.data))], 1, mean)
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, mean)
all.train.data$modeRating <- apply(all.train.data[,2978:2980], 1, Mode)

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
all.train.data$oneVsTwoOutcomeAgeReg <- lm(oneVsTwoOutcome ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$averageRatingAgeReg <- lm(averageRating ~ age + ageSq + sex, data=all.train.data)$residuals

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
                                       cor(as.numeric(meanFSCtAgeReg),
                                           averageRatingAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           averageRatingAgeReg, use='complete', method='spearman'),
                                       cor(CortexVol, averageRatingAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwo <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(CortexVol, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))
detach(all.train.data)
# Now run through the validation data cor values
all.valid.data <- all.valid.data[which(all.valid.data$averageRating.x!=0),]
attach(all.valid.data)
meanValsAgeRegAverageRatingValid <- cbind(cor(meanCTAgeReg, averageRatingAgeReg, method='spearman'),
                                      cor(meanGMDAgeReg, averageRatingAgeReg, method='spearman'),
                                      cor(meanVOLAgeReg, averageRatingAgeReg, method='spearman'),
                                      cor(as.numeric(meanFSCtAgeReg),
                                      averageRatingAgeReg, use='complete', method='spearman'),
                                      cor(as.numeric(meanFSAreaAgeReg),
                                      averageRatingAgeReg, use='complete', method='spearman'),
                                      cor(CortexVol, averageRatingAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwoValid <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                      cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                      cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                      cor(as.numeric(meanFSCtAgeReg),
                                        oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                      cor(as.numeric(meanFSAreaAgeReg),
                                        oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                      cor(CortexVol, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))
detach(all.valid.data)

# Now prepare our values to graph
trainData <- rbind(meanValsAgeRegAverageRating, meanValsAgeRegOneVsTwo)
colnames(trainData) <- c('Ants CT', 'Ants GMD', 'Ants Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(trainData) <- c('Average Rating', 'One vs Two')
trainData <- melt(trainData)
trainData$Var3 <- rep('Train', nrow(trainData))

validData <- rbind(meanValsAgeRegAverageRatingValid, meanValsAgeRegOneVsTwoValid)
colnames(validData) <- c('Ants CT', 'Ants GMD', 'Ants Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(validData) <- c('Average Rating', 'One vs Two')
validData <- melt(validData)
validData$Var3 <- rep('Valid', nrow(validData))

allData <- as.data.frame(rbind(trainData, validData))
allData$Var1 <- as.factor(allData$Var1)
allData$Var2 <- as.factor(allData$Var2)
allData$value <- as.numeric(as.character(allData$value))
allData$Var3 <- as.factor(allData$Var3)
allData$Var2 <- factor(allData$Var2, levels=c('FS CT', 'Ants CT', 'FS Vol', 'Ants Vol', 'Ants GMD', 'FS Area'))

# Now graph our data
thing1 <- ggplot(allData, aes(x=Var2, y=value, color=Var2, fill=Var1, group=Var1)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  theme(legend.position="none") +
  labs(title='', x='Structural Imaging Metric', y='Partial Cor Value') +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  facet_grid(Var3 ~ ., space="free_x")

pdf('partialCorBtn1vs2andAvgRatingFigure12.pdf', width=12, height=16)
thing1
dev.off()
