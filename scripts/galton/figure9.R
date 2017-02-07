# AFGR June 13 2016
# This script is oging to be used to produce the monovariate 1 vs 2 AUC values


## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)

## Load Library(s)
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel')

## Now do the 1 vs 2 model's using the same 0 vs !0 model and then
## Move onto the step wise process for the 1 vs 2 models
## First thing we have to do is prep the data
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

raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value <= 1] <- 0
raw.lme.data$value[raw.lme.data$value > 1] <- 1


# Now go through the same step wise process as I do in the 0 vs !0 data
# Now run through each variable of interest and build an ROC curve for it
outcome <- raw.lme.data$value
qapValNames <- qapValNames[-grep('size', qapValNames)]
qapValNames <- qapValNames[-grep('mean', qapValNames)]
qapValNames <- qapValNames[-grep('std', qapValNames)]
qapValNames <- qapValNames[-grep('fwhm_', qapValNames)]
qapValNames <- qapValNames[-grep('hm.', qapValNames)]
qapValNames <- qapValNames[-grep('all.', qapValNames)]
aucVals <- NULL
for(qapVal in qapValNames){
  model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal, auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals[, 1] <- c("CNR", "EFC", "FBER", "FWHM", "QI1", "SNR", 
 "CSF Kurtosis", "CSF Skewness", "GM Kurtosis", "GM Skewness", 
 "WM Kurtosis", "WM Skewness", "BG Kurtosis", "BG Skewness")


# Now order and find the AUC heirarchy 
aucVals <- as.data.frame(aucVals)
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
aucVals <- aucVals[order(aucVals[,2], decreasing =TRUE),]
aucValsMono <- aucVals
vals <- c(.65, .725)
# Now make a bar plot of all of the 0 vs !0 AUC's
aucZerovsNotZeroMonovariate <- ggplot(aucVals, aes(x=reorder(qapVal, -V2), y=V2)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=vals) +
  ggtitle("") + 
  xlab("Imaging Metrics") +
  ylab("AUC") +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))


png('figure9-monovariateAUC1vs2.png', height=16, width=12, units='in', res=300)
aucZerovsNotZeroMonovariate
dev.off()
