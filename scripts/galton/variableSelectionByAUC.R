# AFGR July 27 2016
# IN this script I am going to be doing some variable selection on the qap data
# The basic method for this is going to be finding an elbow point in the AUC of 
# the multinomial after adding an aditional variable.
# Variables will be added based on their individual AUC for the whole cohort.
# General steps are going to be:
#	1.) Find the AUC's for all of the variables that I am going to use
#	2.) Order those variables
#	3.) Test the AUC for each step of the multinomial function and find the elbow point for improvment

## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)

## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel')

## Now the first thing we need to do is get the AUC for each individual QAP variable 
# In order to do this first thing I need is a data set of all all average rating images 0 vs !0
# Now I need to create the training and validation sets 
mergedQAP$zeroVsNotZero <- mergedQAP$averageRating
mergedQAP$zeroVsNotZero[mergedQAP$zeroVsNOtZero >=1] <- 1

# Now run through each variable of interest and build an ROC curve for it
outcome <- mergedQAP$zeroVsNotZero
outcome[outcome >= 1] <- 1
qapValNames <- qapValNames[-grep('size', qapValNames)]
qapValNames <- qapValNames[-grep('mean', qapValNames)]
qapValNames <- qapValNames[-grep('std', qapValNames)]
qapValNames <- qapValNames[-grep('fwhm_', qapValNames)]
qapValNames <- qapValNames[-grep('hm.', qapValNames)]
qapValNames <- qapValNames[-grep('all.', qapValNames)]
aucVals <- NULL
for(qapVal in qapValNames){
  #predictor <- lm(unname(unlist(mergedQAP[qapVal])) ~ mergedQAP$ageAtGo1Scan, data=mergedQAP)$residuals 
  roc.tmp <- roc(outcome ~ unname(unlist(mergedQAP[qapVal])))
  #roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal, auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

# Now order and find the AUC heirarchy 
aucVals <- as.data.frame(aucVals)
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
aucVals <- aucVals[order(aucVals[,2], decreasing =TRUE),]
# Now make a bar plot of all of the 0 vs !0 AUC's
aucZerovsNotZero <- ggplot(aucVals, aes(x=reorder(qapVal, -V2), y=V2, fill=V2)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  coord_cartesian(ylim=c(.45,.85))

pdf('0vsNot0AllMonomialAUCValues.pdf')
print(aucZerovsNotZero)
dev.off()

# Now create the variable with the names and order of the variables
# I want to add sequentially
reorderedVals <- reorder(aucVals$qapVal, aucVals$V2)
aucNamesOrder <- reorderedVals[1:length(reorderedVals)]


# Now I need to loop through all 36 variables and see how much each 
# training set gains when I add another variable... not sure how I am going to do
# this yet though... =/
# Start by splitting the data into a trinaing and validation set
folds <- createFolds(mergedQAP$zeroVsNotZero, k=3, list=T, returnTrain=T)
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 
 
# Now train across the entire traning sample and validate across
# the entire train valid sample
trainingData.train <- melt(trainingData, id.vars=names(trainingData)[1:32], measure.vars=names(trainingData)[34:36])
trainingData.train$value[trainingData.train$value > 1] <- 1
trainingData.valid <- validationData

# Now build a model in an incremental fashion
aucTrainTrain <- NULL
aucTrainValid <- NULL
aucValidValid <- NULL
valsToUseTest <- NULL
valsToUse <- NULL
for(i in 1:length(aucNamesOrder)){
  if(i == 1){
    valsToUse <- i
    valsToUseTest <- i
  }
  if(i != 1){
    valsToUseTest <- append(valsToUse, i)
  }
  model <- as.formula(paste("value ~", paste(aucNamesOrder[valsToUseTest], collapse="+"), paste("+ (1|variable)")))
  print(model)
  m1 <- glmer(model, data=trainingData.train, family="binomial",
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))

  # Now validate the model on the training training set
  predictedValues <- predict(m1, type='response')
  actualVals <- as.numeric(as.character(trainingData.train$value))
  roc.tmp <- roc(actualVals ~ predictedValues)
  aucTrainTrain <- append(aucTrainTrain, auc(roc.tmp))  

  # Now validate on the training validate data 
  trainingData.valid$variable <- rep('ratingNULL', nrow(trainingData.valid))
  predictedValues <- as.vector(predict(m1, newdata=trainingData.valid, type='response', allow.new.levels=TRUE))
  actualVals <- as.numeric(as.character(trainingData.valid$averageRating.x))
  roc.tmp <- roc(actualVals ~ predictedValues)
  aucTrainValid <- append(aucTrainValid, auc(roc.tmp))
 # Now test to see if we have a significant improvment in ROC
  if(i == 1){
    roc.best <- roc.tmp
  }else{
  #registerDoParallel(cl.1 <- makeCluster(getOption("mc.cores", 4)))
  roc.p.value <- roc.test(roc.best, roc.tmp, alternative="less", method="delong")$p.value#, parallel=F, boot.stratified=T)$p.value
  print(roc.p.value)
  #stopCluster(cl.1)
  # Now Lets check to see if the improvment is any greater!
    if(roc.p.value < .05){
      valsToUse <- append(valsToUse, i)
      roc.best <- roc.tmp
    }
  }
}

# Use this code when output of the foreach for loop is the model selected
model <- as.formula(paste("value ~", paste(aucNamesOrder[valsToUse], collapse="+"), paste("+ (1|variable)")))
m1 <- glmer(model, data=trainingData.train, family="binomial",
      control=glmerControl(optimizer="bobyqa", 
             optCtrl = list(maxfun = 1000000000)))


# Now graph each of the three AUC data points
foo <- seq(1, length(qapValNames))
aucTrainTrain <- as.data.frame(cbind(foo, aucTrainTrain))
aucTrainValid <- as.data.frame(cbind(foo, aucTrainValid))
#aucValidValid <- as.data.frame(cbind(foo, aucValidValid))
 # First start with train train
aucTrainTrainPlot <- ggplot(aucTrainTrain, aes(x=foo, y=aucTrainTrain, fill=aucTrainTrain)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  coord_cartesian(ylim=c(.8,1)) +
  ggtitle("AUC of Training data") + 
  xlab("N of Variables") +
  ylab("AUC")
 # Now do train valid
aucTrainValidPlot <- ggplot(aucTrainValid, aes(x=foo, y=aucTrainValid, fill=aucTrainValid)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  coord_cartesian(ylim=c(.8,1)) +
  ggtitle("AUC of Training Validation Set") + 
  xlab("N of Variables") +
  ylab("AUC")
 # Now do valid valid
#aucValidValidPlot <- ggplot(aucValidValid, aes(x=foo, y=aucValidValid, fill=aucValidValid)) +
#  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
#  theme(axis.text.x = element_text(angle=45,hjust=1)) +
#  coord_cartesian(ylim=c(.8,1)) + 
#  ggtitle("AUC of Validation Set") + 
#  xlab("N of Variables") +
#  ylab("AUC")
# Now print the plots
pdf(paste("trainTrainAucs.pdf", sep=''))
print(aucTrainTrainPlot)
dev.off()
pdf(paste("trainValidAucs.pdf", sep=''))
print(aucTrainValidPlot)
dev.off()
#pdf("validValidAucs.pdf")
#print(aucValidValidPlot)
#dev.off()




