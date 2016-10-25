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
trainingData <- raw.lme.data#[index,]

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


cols <- grep('Meanrelrms', names(all.train.data))
colsCorrect <- cols[c(3, 2, 1, 4)]

val1 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[1], na.rm=T)
colnames(val1)[3] <- 'mean'
val2 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[2], na.rm=T)
colnames(val2)[3] <- 'mean'
val3 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[3], na.rm=T)
colnames(val3)[3] <- 'mean'
val4 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[4], na.rm=T)
colnames(val4)[3] <- 'mean'

# Now lets get the cor btn all of these
newdata1 <- all.train.data[,colsCorrect[1]]
newdata1 <- cbind(newdata1, rep(1, length(newdata1)))
newdata2 <- all.train.data[,colsCorrect[2]]
newdata2 <- cbind(newdata2, rep(2, length(newdata2)))
newdata3 <- all.train.data[,colsCorrect[3]]
newdata3 <- cbind(newdata3, rep(3, length(newdata3)))
newdata4 <- all.train.data[,colsCorrect[4]]
newdata4 <- cbind(newdata4, rep(4, length(newdata4)))
corValData <- rbind(newdata1, newdata2, newdata3, newdata4)
corVal <- cor(corValData[,1], corValData[,2], use='complete')
motionValues <- rbind(val1, val2, val3, val4)
motionValues$.id <- c('ASL', 'nBack', 'iDemo', 'Rest')
motionValues$.id <- factor(motionValues$.id, levels=c('ASL', 'nBack', 'iDemo', 'Rest'))

pdf('timeInScannervsMeanMotion.pdf')
ggplot(motionValues, aes(x=.id, y=mean, fill=.id)) +
geom_bar(stat='identity', position=position_dodge(), size=.1) +
geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
width = .2, position=position_dodge(.9)) +
theme_bw() +
theme(legend.position="none") +
labs(title='', x='Scan Series', y='Mean Rel RMS') +
coord_cartesian(ylim=c(.1,.17)) +
theme(text=element_text(size=20), axis.text.x = element_text(angle = 0))
dev.off()


# Now plot the difference amongst imaging metrics vs the manual ratings
all.train.data$meanCT <- apply(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], 1, mean)
all.train.data$meanCTAgeReg <- lm(meanCT ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanGMD <- apply(all.train.data[,grep('mprage_jlf_gmd', names(all.train.data))], 1, mean)
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, mean)
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ ageAtGo1Scan + sex, data=all.train.data)$residuals
all.train.data$meanFSArea <- apply(all.train.data[,215:282], 1, function(x) mean(x, na.rm=T))

# Now lets plot these values
meanCTValues <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='meanCTAgeReg', na.rm=T)
meanGMDValues <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='meanGMDAgeReg', na.rm=T)
meanVOLValues <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='meanVOLAgeReg', na.rm=T)
meanFSVOlValues <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='meanFSArea', na.rm=T)

# Now plot these effers 
thing1 <- ggplot(meanCTValues, aes(x=as.factor(averageRating), y=meanCTAgeReg, group=as.factor(averageRating))) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  geom_errorbar(aes(ymin=meanCTAgeReg-se, ymax=meanCTAgeReg+se),
  width = .2, position=position_dodge(.9)) +
  theme(legend.position="none") +
  labs(title='', x='Average Rating Bin', y='Mean Age Regressed CT Values') + 
  coord_cartesian(ylim=c(-.2,.1)) +
  theme(axis.text.x = element_text(angle=0,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) 

thing2 <- ggplot(meanGMDValues, aes(x=as.factor(averageRating), y=meanGMDAgeReg, group=as.factor(averageRating))) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  geom_errorbar(aes(ymin=meanGMDAgeReg-se, ymax=meanGMDAgeReg+se),
  width = .2, position=position_dodge(.9)) +
  theme(legend.position="none") +
  labs(title='', x='Average Rating Bin', y='Mean Age Regressed GMD Values') + 
  coord_cartesian(ylim=c(-.15,.05)) +
  theme(axis.text.x = element_text(angle=0,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) 

thing3 <- ggplot(meanVOLValues, aes(x=as.factor(averageRating), y=meanVOLAgeReg, group=as.factor(averageRating))) + 
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
  geom_errorbar(aes(ymin=meanVOLAgeReg-se, ymax=meanVOLAgeReg+se),
  width = .2, position=position_dodge(.9)) +
  theme(legend.position="none") +
  labs(title='', x='Average Rating Bin', y='Mean Age Regressed VOL Values') +
  theme(axis.text.x = element_text(angle=0,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))

pdf('manualMeanQualityVsImagingMetrics.pdf', height=12, width=12)
thing1
thing2
thing3
dev.off()

