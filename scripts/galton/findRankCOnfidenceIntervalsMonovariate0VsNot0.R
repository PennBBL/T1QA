## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

## Load Library(s)
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel')
# Now split data into raw and traning 
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)


# Now run through each variable of interest and build an ROC curve for it
qapValNames <- qapValNames[-grep('size', qapValNames)]
qapValNames <- qapValNames[-grep('mean', qapValNames)]
qapValNames <- qapValNames[-grep('std', qapValNames)]
qapValNames <- qapValNames[-grep('fwhm_', qapValNames)]
qapValNames <- qapValNames[-grep('hm.', qapValNames)]
qapValNames <- qapValNames[-grep('all.', qapValNames)]

raw.lme.data.orig <- raw.lme.data

cl <- makeCluster(20)
registerDoParallel(cl)
outputRanks <- foreach(i=seq(1,1000), .combine='rbind') %dopar% {
  library('lme4')
  library('pROC')
  library('caret')
  set.seed(i)
  # Lets first create our fold
  folds <- createFolds(raw.lme.data.orig$averageRating.x, k=3, list=T, returnTrain=T)
  
  # Now lets create our training data set
  index <- unlist(folds[1])
  trainingData <- raw.lme.data.orig[index,]
  validationData <- raw.lme.data.orig[-index,] 
  raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data.orig)[1:32], measure.vars=names(raw.lme.data.orig)[34:36])
  raw.lme.data$value[raw.lme.data$value > 1] <- 1
  outcome <- raw.lme.data$value

  # Now create all of our glmer models
  aucVals <- NULL
  for(qapVal in qapValNames){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    m1 <- glmer(model, data=raw.lme.data, family="binomial")
    predictor <- predict(m1, type='response')
    roc.tmp <- roc(outcome ~ predictor)
    output <- cbind(qapVal, auc(roc.tmp))
    aucVals <- rbind(aucVals, output)
  }
  order(aucVals[,2], decreasing =TRUE)
}
stopCluster(cl)

write.csv(outputRanks, 'ranks1000RepsMono.csv', quote=F)

# Now do the bivariate model
qapValNames <- qapValNames[-13]
cl <- makeCluster(20)
registerDoParallel(cl)
outputRanks <- foreach(i=seq(1,1000), .combine='rbind') %dopar% {
  library('lme4')
  library('pROC')
  library('caret')
  set.seed(i)
  # Lets first create our fold
  folds <- createFolds(raw.lme.data.orig$averageRating.x, k=3, list=T, returnTrain=T)
  
  # Now lets create our training data set
  index <- unlist(folds[1])
  trainingData <- raw.lme.data.orig[index,]
  validationData <- raw.lme.data.orig[-index,] 
  raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data.orig)[1:32], measure.vars=names(raw.lme.data.orig)[34:36])
  raw.lme.data$value[raw.lme.data$value > 1] <- 1
  outcome <- raw.lme.data$value

  # Now create all of our glmer models
  aucVals <- NULL
  for(qapVal in qapValNames){
    model <- as.formula(paste("value ~ bg.kurtosis + ", paste(qapVal), paste("+ (1|variable)")))
    m1 <- glmer(model, data=raw.lme.data, family="binomial")
    predictor <- predict(m1, type='response')
    roc.tmp <- roc(outcome ~ predictor)
    output <- cbind(qapVal, auc(roc.tmp))
    aucVals <- rbind(aucVals, output)
  }
  order(aucVals[,2], decreasing =TRUE)
}
stopCluster(cl)

write.csv(outputRanks, 'ranks1000RepsBi.csv', quote=F)


