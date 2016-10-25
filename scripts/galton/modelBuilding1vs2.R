# AFGR August 6 2016
# This script is going to mirror the step wise model building script but perform the same process in the 1 vs 2 cohort

## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)

## Declare any functions
rocdata <- function(grp, pred){
  # Produces x and y co-ordinates for ROC curve plot
  # Arguments: grp - labels classifying subject status
  #            pred - values of each observation
  # Output: List with 2 components:
  #         roc = data.frame with x and y co-ordinates of plot
  #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
 
  grp <- as.factor(grp)
  if (length(pred) != length(grp)) {
    stop("The number of classifiers must match the number of data points")
  } 
 
  if (length(levels(grp)) != 2) {
    stop("There must only be 2 values for the classifier")
  }
 
  cut <- unique(pred)
  tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
  fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
  fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
  tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  roc = data.frame(x = fpr, y = tpr)
  roc <- roc[order(roc$x, roc$y),]
 
  i <- 2:nrow(roc)
  auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
 
  pos <- pred[grp == levels(grp)[2]]
  neg <- pred[grp == levels(grp)[1]]
  q1 <- auc/(2-auc)
  q2 <- (2*auc^2)/(1+auc)
  se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
  ci.upper <- auc + (se.auc * 0.96)
  ci.lower <- auc - (se.auc * 0.96)
 
  se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
  z <- (auc - 0.5)/se.auc.null
  p <- 2*pnorm(-abs(z))
 
  stats <- data.frame (auc = auc,
                       p.value = p,
                       ci.upper = ci.upper,
                       ci.lower = ci.lower
                       )
 
  return (list(roc = roc, stats = stats))
}


# Create a function which will plot a roc curve in ggplot
rocplot.single <- function(grp, pred, title = "ROC Plot", p.value = FALSE){
  require(ggplot2)
  plotdata <- rocdata(grp, pred)
 
  if (p.value == TRUE){
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
  } else {
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95%CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
  }
 
  p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
      geom_line(aes(colour = "")) +
      geom_abline (intercept = 0, slope = 1) +
      theme_bw() +
      scale_x_continuous("False Positive Rate (1-Specificity)") +
      scale_y_continuous("True Positive Rate (Sensitivity)") +
      scale_colour_manual(labels = annotation, values = "#000000") +
      ggtitle(title) +
      theme_bw() + 
      theme(legend.position=c(1,0)) +
      theme(legend.justification=c(1,0)) +
      theme(legend.title=element_blank())
  return(p)
}

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
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value <= 1] <- 0
raw.lme.data$value[raw.lme.data$value > 1] <- 1

## First check how the 0 vs !0 model performs in the 1 vs 2 data
# Load the 0 vs !0 model here
load(file='/home/adrose/qapQA/data/0vsNot0FinalData.RData')
predictor <- predict(m1, newdata=raw.lme.data, type='response')
outcome <- raw.lme.data$value
roc.1.vs.2.go.0.vs.not.0.model <- rocplot.single(outcome, predictor, title="1 vs 2 Using 0 vs !0 Model")

# Now train with the same model variables just in the new data
model1 <- as.formula("value ~ bg.kurtosis+bg.skewness+ (1|variable)")
m1 <- glmer(model1, data=raw.lme.data, family="binomial")
# Now test this model's performance
predictor <- predict(m1, type='response')
outcome <- raw.lme.data$value
roc.1.vs.2.go.train.in <- rocplot.single(outcome, predictor, title="1 vs 2 Using 1 vs 2 Model")

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

# Now order and find the AUC heirarchy 
aucVals <- as.data.frame(aucVals)
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
aucVals <- aucVals[order(aucVals[,2], decreasing =TRUE),]
aucValsMono <- aucVals
# Now make a bar plot of all of the 0 vs !0 AUC's
aucZerovsNotZeroMonovariate <- ggplot(aucVals, aes(x=reorder(qapVal, -V2), y=V2)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.60,.75)) +
  ggtitle("") + 
  xlab("QAP Variables") +
  ylab("AUC") +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))
# Now order the varaibles based on AUC
reorderedVals <- reorder(aucVals$qapVal, aucVals$V2)
aucNamesOrder <- reorderedVals[1:length(reorderedVals)]

aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the bivariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste("qi1", qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal, paste("qi1", qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

# Now plot them based on order
aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsBi <- aucVals
# Now make a bar plot of all of the 0 vs !0 AUC's
aucZerovsNotZeroBivariate <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Bivariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")


# Now do the third variable
#reorderedVals <- reorder(aucVals$qapVal, aucVals$V3)
#aucNamesOrder <- reorderedVals[1:length(reorderedVals)]
# Now run through and build each model
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the bivariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsTri <- aucVals
aucZerovsNotZeroTrivaraite <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Trivariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")



# Now do a quick check to see if we are gaining any significance by adding more variables
model1 <- as.formula(paste("value ~", paste(aucValsMono$qapVal[1]), paste("+ (1|variable)")))
mod1 <- glmer(model1, data=raw.lme.data, family="binomial")
predictor1 <- predict(mod1)
roc.1 <- roc(outcome ~ predictor1)
model2 <- as.formula(paste("value ~", paste(aucValsBi$V2[1]), paste("+ (1|variable)")))
mod2 <- glmer(model2, data=raw.lme.data, family="binomial")
predictor2 <- predict(mod2)
roc.2 <- roc(outcome ~ predictor2)
model3 <- as.formula(paste("value ~", paste(aucValsTri$V2[1]), paste("+ (1|variable)")))
mod3 <- glmer(model3, data=raw.lme.data, family="binomial")
predictor3 <- predict(mod3)
roc.3 <- roc(outcome ~ predictor3)


# Now do the fourth variable
#reorderedVals <- reorder(aucVals$qapVal, aucVals$V3)
#aucNamesOrder <- reorderedVals[1:length(reorderedVals)]
# Now run through and build each model
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsQuad <- aucVals
aucZerovsNotZeroTetra <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Tetravariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 5 variables
#reorderedVals <- reorder(aucVals$qapVal, aucVals$V3)
#aucNamesOrder <- reorderedVals[1:length(reorderedVals)]
# Now run through and build each model
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsPenta <- aucVals
aucZerovsNotZeroPenta <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Tetravariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 6 variables
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsHexa <- aucVals
aucZerovsNotZeroHexa <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Hexavariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 7 variables
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsHepta <- aucVals
aucZerovsNotZeroHepta <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Heptavariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 8 variables
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsOcta <- aucVals
aucZerovsNotZeroOcta <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Octavariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 9 variables
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsNona <- aucVals
aucZerovsNotZeroNona <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Nonavariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")

# Now do 10 variables
modelToBuildOn <- aucVals$V2[1]
aucNamesOrder <- aucNamesOrder[-grep(aucVals$qapVal[1], aucNamesOrder)]
aucVals <- NULL
# Now do the tetravariate addition
for(qapVal in aucNamesOrder[1:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
  predictor <- predict(m1, type='response')
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal,  paste(modelToBuildOn, qapVal, sep='+'), auc(roc.tmp))
  aucVals <- rbind(aucVals, output)
}

aucVals <- as.data.frame(aucVals)
aucVals$V3 <- as.numeric(as.character(aucVals$V3))
aucVals <- aucVals[order(aucVals[,3], decreasing =TRUE),]
aucValsDeca <- aucVals
aucZerovsNotZeroDeca <- ggplot(aucVals, aes(x=reorder(V2, -V3), y=V3)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(.65,.85)) +
  ggtitle("AUC Of Decavariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")







pdf('forwardStepAUCModels1-8.pdf')
aucZerovsNotZeroMonovariate  
aucZerovsNotZeroBivariate 
aucZerovsNotZeroTrivaraite
aucZerovsNotZeroTetra
aucZerovsNotZeroPenta
aucZerovsNotZeroHexa 
aucZerovsNotZeroHepta
aucZerovsNotZeroOcta
aucZerovsNotZeroNona
aucZerovsNotZeroDeca
dev.off() 



# Now find all of our significant increases
model1 <- as.formula(paste("value ~", paste(aucValsMono$qapVal[1]), paste("+ (1|variable)")))
mod1 <- glmer(model1, data=raw.lme.data, family="binomial")
predictor1 <- predict(mod1)
roc.1 <- roc(outcome ~ predictor1)
model2 <- as.formula(paste("value ~", paste(aucValsBi$V2[1]), paste("+ (1|variable)")))
mod2 <- glmer(model2, data=raw.lme.data, family="binomial")
predictor2 <- predict(mod2)
roc.2 <- roc(outcome ~ predictor2)
model3 <- as.formula(paste("value ~", paste(aucValsTri$V2[1]), paste("+ (1|variable)")))
mod3 <- glmer(model3, data=raw.lme.data, family="binomial")
predictor3 <- predict(mod3)
roc.3 <- roc(outcome ~ predictor3)
model4 <- as.formula(paste("value ~", paste(aucValsQuad$V2[1]), paste("+ (1|variable)")))
mod4 <- glmer(model4, data=raw.lme.data, family="binomial")
predictor4 <- predict(mod4)
roc.4 <- roc(outcome ~ predictor4)
model5 <- as.formula(paste("value ~", paste(aucValsPenta$V2[1]), paste("+ (1|variable)")))
mod5 <- glmer(model5, data=raw.lme.data, family="binomial")
predictor5 <- predict(mod5)
roc.5 <- roc(outcome ~ predictor5)
model6 <- as.formula(paste("value ~", paste(aucValsHexa$V2[1]), paste("+ (1|variable)")))
mod6 <- glmer(model6, data=raw.lme.data, family="binomial")
predictor6 <- predict(mod6)
roc.6 <- roc(outcome ~ predictor6)
model7 <- as.formula(paste("value ~", paste(aucValsHepta$V2[1]), paste("+ (1|variable)")))
mod7 <- glmer(model7, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
predictor7 <- predict(mod7)
roc.7 <- roc(outcome ~ predictor7)
model8 <- as.formula(paste("value ~", paste(aucValsOcta$V2[1]), paste("+ (1|variable)")))
mod8 <- glmer(model8, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
predictor8 <- predict(mod8, type='response')
roc.8 <- roc(outcome ~ predictor8)
model9 <- as.formula(paste("value ~", paste(aucValsNona$V2[1]), paste("+ (1|variable)")))
mod9 <- glmer(model9, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
predictor9 <- predict(mod9)
roc.9 <- roc(outcome ~ predictor9)
model10 <- as.formula(paste("value ~", paste(aucValsDeca$V2[1]), paste("+ (1|variable)")))
mod10 <- glmer(model10, data=raw.lme.data, family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 1000000000)))
predictor10 <- predict(mod10)
roc.10 <- roc(outcome ~ predictor10)


p1 <- roc.test(roc.1, roc.2, alternative='less')$p.value
p2 <- roc.test(roc.2, roc.3, alternative='less')$p.value
p3 <- roc.test(roc.3, roc.4, alternative='less')$p.value
p4 <- roc.test(roc.4, roc.5, alternative='less')$p.value
p5 <- roc.test(roc.5, roc.6, alternative='less')$p.value
p6 <- roc.test(roc.6, roc.7, alternative='less')$p.value
p7 <- roc.test(roc.7, roc.8, alternative='less')$p.value
p8 <- roc.test(roc.8, roc.9, alternative='less')$p.value
p9 <- roc.test(roc.9, roc.10, alternative='less')$p.value

pVals <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9)
testVals <- rbind(c("Mono vs Bi"), c("Bi vs Tri"), c("Tri vs Quad"), c("Quad vs Penta"), 
                  c("Penta vs Hexa"), c("Hexa vs Hepta"), c("Hepta vs Octa"), c("Octa vs Nona"), c("Nona vs Deca"))
pVals <- as.data.frame(cbind(testVals, pVals))
levels(pVals$V1) <- pVals$V1
pVals$V1 <- as.factor(levels(pVals$V1))
levels(pVals$V1) <- pVals$V1
pVals$Z <- as.numeric(as.character(pVals$Z))
pVals$Y <- c(p4, p1, p9, p8, p7, p3, p2, p6, p5)
pVals <- pVals[-c(7,2),]
pValsBarGraph <- ggplot(pVals, aes(x=V1, y=Y)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  coord_cartesian(ylim=c(0,.3)) +
  ggtitle("") + 
  xlab("Model") +
  ylab("P Value") + 
  geom_hline(yintercept=.05) + 
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30))

pdf("pValsBarGraph.pdf", width=12, height=12)
print(pValsBarGraph)
dev.off()


source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$variable <- rep("ratingNULL", nrow(raw.lme.data))
predictor <- predict(mod8, newdata=raw.lme.data, allow.new.levels=T, type='response')
outcome <- raw.lme.data$averageRating.x
roc.tmp <- roc(outcome ~ predictor)
pdf(
