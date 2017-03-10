## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalDataRev.RData')
zeroVsNotZeroModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModelRev.RData')
oneVsTwoModel <- mod8
rm(mod8)


## Now load the library(s)
install_load('caret', 'pROC', 'ggplot2', 'doParallel')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
raw.lme.data$averageRating.y[raw.lme.data$averageRating.y<1.5] <- 1
raw.lme.data$averageRating.y[raw.lme.data$averageRating.y>1.5] <- 2
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now prepare our model prediciton output
all.train.data$variable <- rep('ratingNULL', nrow(trainingData))
all.valid.data$variable <- rep('ratingNULL', nrow(validationData))
all.train.data$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=all.train.data,
allow.new.levels=T, type='response')
all.valid.data$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=all.valid.data,
allow.new.levels=T, type='response')

all.train.data$zeroVsNotZeroOutcome <- predict(zeroVsNotZeroModel, newdata=all.train.data, allow.new.levels=T, type='response')
all.valid.data$zeroVsNotZeroOutcome <- predict(zeroVsNotZeroModel, newdata=all.valid.data, allow.new.levels=T, type='response')

# Now make the train and valid outcomes for us to compare our delong tests to
zeroVsNotZeroTrainModel <- roc(averageRating.x ~ zeroVsNotZeroOutcome, data=all.train.data)
zeroVsNotZeroValidModel <- roc(averageRating.x ~ zeroVsNotZeroOutcome, data=all.valid.data)
oneVsTwoTrainModel <- roc(averageRating.y ~ oneVsTwoOutcome, data=all.train.data)
oneVsTwoValidModel <- roc(averageRating.y ~ oneVsTwoOutcome, data=all.valid.data)

# Now prepare the column and row names
namesNew <- c('tfMRI 1', 'tfMRI 2', 'PCASL', 'rsfMRI')

# Now get the motion cols
motionCols <- grep('Meanrelrms', names(all.train.data))

# Now create an ROC curve for each of the motion metrics first for 0 vs !0
outputVal <- all.train.data$averageRating.x
aucVals <- NULL
pVals <- NULL
w <- 1
for(i in motionCols){
  tmp.vals <- all.train.data[,i]
  roc.tmp <- roc(outputVal~tmp.vals)
  pVal <- roc.test(roc.tmp, zeroVsNotZeroTrainModel, method='d', alternative='greater')$p.value
  output <- cbind(namesNew[w], auc(roc.tmp), c('Train'), c('0 vs !0'))
  aucVals <- rbind(aucVals, output)
  pVals <- rbind(pVals, pVal)
  w <- w + 1
}
pValsZeroTrain <- pVals
aucVals <- as.data.frame(aucVals)
aucVals$V1 <- factor(aucVals$V1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
trainZeroNotZeroBG <- ggplot(aucVals, aes(x=V1, y=V2)) + 
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.6,1)) +
  ggtitle("Training") +
  geom_hline(yintercept=.94, linetype="longdash", colour="black", size=0.5) + 
  xlab("Sequence") +
  ylab("AUC") +
  theme_bw()

aucVals$V5 <- rep('0 vs !0', 4)
aucVals$V6 <- rep('Training', 4)
aucValsAll <- aucVals
outputVal <- all.valid.data$averageRating.x
w <- 1
aucVals <- NULL
pVals <- NULL
for(i in motionCols){
  tmp.vals <- all.valid.data[,i]
  roc.tmp <- roc(outputVal~tmp.vals)
  pVal <- roc.test(roc.tmp, zeroVsNotZeroValidModel, method='d', alternative='greater')$p.value
  output <- cbind(namesNew[w], auc(roc.tmp), c('Valid'), c('0 vs !0'))
  aucVals <- rbind(aucVals, output)
  pVals <- rbind(pVals, pVal)
  w <- w + 1
}
pValsZeroValid <- pVals
aucVals <- as.data.frame(aucVals)
aucVals$V1 <- factor(aucVals$V1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
validZeroNotZeroBG <- ggplot(aucVals, aes(x=V1, y=as.numeric(as.character(V2)))) + 
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.6,1)) +
  ggtitle("Validation") +
  geom_hline(yintercept=.96, linetype="longdash", colour="black", size=0.5) + 
  xlab("Sequence") +
  ylab("AUC") + 
  theme_bw()

aucVals$V5 <- rep('0 vs !0', 4)
aucVals$V6 <- rep('Validation', 4)
aucValsAll <- rbind(aucValsAll, aucVals)


# Now do the 1 vs 2 data 
set.seed(16)
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse1vs2.RData')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

# Now set the train and validation data
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# now produce the training 1 vs 2 AUC values 
outputVal <- all.train.data$averageRating.x
w <- 1
aucVals <- NULL
pVals <- NULL
for(i in motionCols){
  tmp.vals <- all.train.data[,i]
  roc.tmp <- roc(outputVal~tmp.vals)
  pVal <- roc.test(roc.tmp, oneVsTwoTrainModel, method='d')$p.value
  output <- cbind(namesNew[w], auc(roc.tmp), c('Train'), c('1 vs 2'))
  aucVals <- rbind(aucVals, output)
  pVals <- rbind(pVals, pVal)
  w <- w + 1
}
pValsOneTrain <- pVals
aucVals <- as.data.frame(aucVals)
aucVals$V1 <- factor(aucVals$V1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
trainOneVsTwoBG <- ggplot(aucVals, aes(x=V1, y=as.numeric(as.character(V2)))) + 
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.6,1)) +
  ggtitle("") + 
  geom_hline(yintercept=.87, linetype="longdash", colour="black", size=0.5) + 
  xlab("Sequence") +
  ylab("AUC") + 
  theme_bw()


aucVals$V5 <- rep('1 vs 2', 4)
aucVals$V6 <- rep('Training', 4)
aucValsAll <- rbind(aucValsAll, aucVals)

# Now do the 1 vs 2 validation data
outputVal <- all.valid.data$averageRating.x
w <- 1
aucVals <- NULL
pVals <- NULL
for(i in motionCols){
  tmp.vals <- all.valid.data[,i]
  roc.tmp <- roc(outputVal~tmp.vals)
  pVal <- roc.test(roc.tmp, oneVsTwoTrainModel, method='d')$p.value
  output <- cbind(namesNew[w], auc(roc.tmp), c('Valid'), c('1 vs 2'))
  aucVals <- rbind(aucVals, output)
  pVals <- rbind(pVals, pVal)
  w <- w + 1
}
pValsOneValid <- pVals
aucVals <- as.data.frame(aucVals)
aucVals$V1 <- factor(aucVals$V1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
validOneVsTwoBG <- ggplot(aucVals, aes(x=V1, y=as.numeric(as.character(V2)))) + 
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.6,1)) +
  ggtitle("") + 
  geom_hline(yintercept=.78, linetype="longdash", colour="black", size=0.5) + 
  xlab("Sequence") +
  ylab("AUC") + 
  theme_bw()

aucVals$V5 <- rep('1 vs 2', 4)
aucVals$V6 <- rep('Validation', 4)
aucValsAll <- rbind(aucValsAll, aucVals)

horizontalDataFrame <- as.data.frame(cbind(c('0 vs !0', '0 vs !0', '1 vs 2', '1 vs 2'),
                             c('Training', 'Validation', 'Training', 'Validation'),
                             c(.94, .96, .87, .78)))
colnames(horizontalDataFrame) <- c('V5', 'V6', 'V7')
horizontalDataFrame$V7 <- as.numeric(as.character(horizontalDataFrame$V7))

thing1 <- ggplot(aucValsAll, aes(x=V1, y=as.numeric(as.character(V2)))) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme_bw() +
  theme(legend.position="none",
  axis.text.x = element_text(angle=90,hjust=1, size=26, face="bold"),
  axis.text.y = element_text(size=26, face="bold"),
  axis.title=element_text(size=36,face="bold"),
  strip.text.y = element_text(size = 30, angle = 270, face="bold"),
  strip.text.x = element_text(size = 30, angle = 0, face="bold")) +
  coord_cartesian(ylim=c(.6,1)) +
  geom_hline(data=horizontalDataFrame, aes(yintercept=V7), linetype="longdash", colour="black", size=0.5) +
  xlab("Sequence") +
  ylab("AUC") +
  facet_grid(V5 ~ V6)


# Now plot all of this stuff
png('figure16-motionAsIdentifierAUCValues.png', height=20, width=20, units='in', res=300)
thing1
dev.off()
