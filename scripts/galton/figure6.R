## Load the data
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalData.RData')
zeroVsNotZeroModel <- m1
rm(m1)

## Load Library(s)
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'grid', 'gridExtra')

## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse1vs2.RData')
#raw.lme.data[,2:33] <- scale(raw.lme.data[,2:33], center=T, scale=T)
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
trainingData <- trainingData[complete.cases(trainingData),]
validationData <- raw.lme.data[-index,]
validationData <- validationData[complete.cases(validationData),]

# Now get the measure vars
measureVars <- names(raw.lme.data)[1:33]
# Now get the id.vars
idVars <- names(raw.lme.data)[35:37]

raw.lme.data <- melt(trainingData, id.vars=measureVars, measure.vars=idVars)
raw.lme.data$value[raw.lme.data$value <= 1] <- 0
raw.lme.data$value[raw.lme.data$value >= 1] <- 1
raw.lme.data.test <- melt(validationData, id.vars=measureVars, measure.vars=idVars)
raw.lme.data.test$value[raw.lme.data.test$value <= 1] <- 0
raw.lme.data.test$value[raw.lme.data.test$value >= 1] <- 1


# Now go through the same step wise process as I do in the 0 vs !0 data
# Now run through each variable of interest and build an ROC curve for it
qapValNamesUse <- qapValNames[-c(1:3, 5:7, 10:12, 14:19, 22:26,33:34)]
qapValNamesUse <- c('bg.kurtosis', 'bg.skewness', 'cnr', 'efc', 'fber', 'qi1', 'snr', 'wm.skewness', 'mean_euler')
aucVals <- NULL
testTmpSet <- validationData
testTmpSet$variable <- 'ratingNULL'
validTmpSet <- all.mgi.data[c(qapValNames, 'averageRating')]
validTmpSet <- validTmpSet[-which(validTmpSet$averageRating==0),]
validTmpSet$averageRating[validTmpSet$averageRating<1.5] <- 0
validTmpSet$averageRating[validTmpSet$averageRating>1.5] <- 1
validTmpSet$variable <- 'ratingNULL'
for(qapVal in qapValNamesUse){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    # NOw train our model in the training data
    m1 <- glmer(model, data=raw.lme.data, family='binomial')
    # Now get our auc vals
    trainAUC <- auc(roc(raw.lme.data$value ~ predict(m1, type='response')))
    # Now get our test value auc
    testAUC <- auc(roc(testTmpSet$averageRating.x ~ predict(m1, type='response', newdata=testTmpSet, allow.new.levels=T)))
    validAUC <- auc(roc(validTmpSet$averageRating ~ predict(m1, type='response', newdata=validTmpSet, allow.new.levels=T)))
    # Now prepare the output
    outputData <- rbind(cbind(trainAUC, 'Training', qapVal), cbind(testAUC, 'Testing', qapVal), cbind(validAUC, 'Validation', qapVal))
    aucVals <- rbind(aucVals, outputData)
}
aucValsAll <- aucVals
aucValsAll <- cbind(aucVals, rep('Training', 27))

# Now train the data in the testing data set
aucVals <- NULL
trainTmpSet <- trainingData
trainTmpSet$variable <- 'ratingNULL'
for(qapVal in qapValNamesUse){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    # NOw train our model in the training data
    m1 <- glmer(model, data=raw.lme.data.test, family='binomial')
    # Now get our auc vals
    testAUC <- auc(roc(raw.lme.data.test$value ~ predict(m1, type='response')))
    # Now get our test value auc
    trainAUC <- auc(roc(trainTmpSet$averageRating.x ~ predict(m1, type='response', newdata=trainTmpSet, allow.new.levels=T)))
    validAUC <- auc(roc(validTmpSet$averageRating ~ predict(m1, type='response', newdata=validTmpSet, allow.new.levels=T)))
    # Now prepare the output
    outputData <- rbind(cbind(trainAUC, 'Training', qapVal), cbind(testAUC, 'Testing', qapVal), cbind(validAUC, 'Validation', qapVal))
    aucVals <- rbind(aucVals, outputData)
}
aucVals <- cbind(aucVals, rep('Testing', 27))
aucValsAll <- rbind(aucValsAll, aucVals)



source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
raw.lme.data.mgi <- merge(isolatedVars, manualQAData2, by=intersect(names(isolatedVars), names(manualQAData2)))
#raw.lme.data.mgi[,4:35] <- scale(raw.lme.data.mgi[,4:35], center=T, scale=T)
raw.lme.data.mgi <- raw.lme.data.mgi[-which(raw.lme.data.mgi$rawAverageRating<.9),]
raw.lme.data.mgi <- melt(raw.lme.data.mgi, id.vars=measureVars, measure.vars=idVars)
raw.lme.data.mgi <- raw.lme.data.mgi[complete.cases(raw.lme.data.mgi),]
raw.lme.data.mgi$value[raw.lme.data.mgi$value==1] <- 0
raw.lme.data.mgi$value[raw.lme.data.mgi$value==2] <- 1

# Now produce our AUC vals after training in the validation data set!
aucVals <- NULL
for(qapVal in qapValNamesUse){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    # NOw train our model in the training data
    m1 <- glmer(model, data=raw.lme.data.mgi, family='binomial')
    # Now get our auc vals
    validAUC <- auc(roc(raw.lme.data.mgi$value ~ predict(m1, type='response')))
    # Now get our test value auc
    testAUC <- auc(roc(testTmpSet$averageRating.x ~ predict(m1, type='response', newdata=testTmpSet, allow.new.levels=T)))
    trainAUC <- auc(roc(trainTmpSet$averageRating.x ~ predict(m1, type='response', newdata=trainTmpSet, allow.new.levels=T)))
    # Now prepare the output
    outputData <- rbind(cbind(trainAUC, 'Training', qapVal), cbind(testAUC, 'Testing', qapVal), cbind(validAUC, 'Validation', qapVal))
    aucVals <- rbind(aucVals, outputData)
}
aucVals <- cbind(aucVals, rep('Validation', 27))
aucValsAll <- rbind(aucValsAll, aucVals)

# Now create our data frame to plot
aucVals <- as.data.frame(aucValsAll)
levels(aucVals$V2) <- c('Training', 'Testing', 'Validation')
levels(aucVals$V4) <- c('Training', 'Testing', 'Validation')
aucVals$trainAUC <- as.numeric(as.character(aucVals$trainAUC))
aucVals$prettyQap <- rep(rep(c('BG Kurtosis', 'BG Skewness', 'CNR', 'EFC', 'FBER', 'QI1', 'SNR', 'WM Skewness', 'Mean Euler'), each=3), 3)
aucVals$prettyQap <- factor(aucVals$prettyQap, levels=c('QI1', 'EFC', 'WM Skewness', 'SNR', 'CNR', 'FBER', 'BG Skewness', 'BG Kurtosis', 'Mean Euler'))
aucValPlot <- ggplot(aucVals, aes(x=prettyQap, y=trainAUC)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  coord_cartesian(ylim=c(.5,.9)) +
  facet_grid(V2 ~ V4, scales="free", space="free_x") +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=20),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(size=30),
    text = element_text(size=30),
    panel.margin = unit(1, "lines")) +
  ggtitle("AUC across various training scheme") +
  xlab("Image Quality Metrics") +
  ylab("AUC")

png('figure6-AUCAcrossTrain.png', width=20, height=20, units='in', res=300)
print(aucValPlot)
dev.off()

# Now create the model to validate
modelToTrain <- as.formula("value ~ mean_euler + (1|variable)")
mOut <- glmer(modelToTrain, data=raw.lme.data, family="binomial")
save(mOut, file="/home/adrose/1vs2EulerMixedModel.RData")
