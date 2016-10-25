# AFGR August 4 2016
# This script is going to be used to go through the variable selectin process starting at one varibale, then 2 variables,
# Then finally show the three varibale selection process


## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
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
      #theme(axis.title.x = theme_text(face="bold", size=12)) +
      #theme(axis.title.y = theme_text(face="bold", size=12, angle=90)) +
      theme(legend.position=c(1,0)) +
      theme(legend.justification=c(1,0)) +
      theme(legend.title=element_blank())
      

     #theme(title = title,
           #plot.title = theme_text(face="bold", size=14), 
           #axis.title.x = theme_text(face="bold", size=12),
           #axis.title.y = theme_text(face="bold", size=12, angle=90),
           #panel.grid.major = theme_blank(),
           #panel.grid.minor = theme_blank(),
           #legend.justification=c(1,0), 
           #legend.position=c(1,0),
           #legend.title=theme_blank(),
           #legend.key = theme_blank()
           #)
  return(p)
}


## Load Library(s)
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'foreach', 'doParallel')

# Now split data into raw and traning 
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value > 1] <- 1

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
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.6,.95)) +
  ggtitle("") + 
  xlab("QAP Variables") +
  ylab("AUC")


# Now order the varaibles based on AUC
reorderedVals <- reorder(aucVals$qapVal, aucVals$V2)
aucNamesOrder <- reorderedVals[1:length(reorderedVals)]

aucVals <- NULL
# Now do the bivariate addition
for(qapVal in aucNamesOrder[2:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(aucNamesOrder[1], qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1)
  roc.tmp <- roc(outcome ~ predictor)
  output <- cbind(qapVal, paste(aucNamesOrder[1], qapVal, sep='+'), auc(roc.tmp))
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
  theme(axis.text.x = element_text(angle=90,hjust=1, size=30), 
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        text = element_text(size=30)) +
  coord_cartesian(ylim=c(.8,1)) +
  ggtitle("") + 
  xlab("QAP Variables") +
  ylab("AUC")


# Now do the third variable
reorderedVals <- reorder(aucVals$qapVal, aucVals$V3)
aucNamesOrder <- reorderedVals[1:length(reorderedVals)]
# Now run through and build each model
modelToBuildOn <- aucVals$V2[1]
aucVals <- NULL
# Now do the bivariate addition
for(qapVal in aucNamesOrder[3:length(aucNamesOrder)]){
  model <- as.formula(paste("value ~", paste(modelToBuildOn, qapVal, sep='+'), paste("+ (1|variable)")))
  m1 <- glmer(model, data=raw.lme.data, family="binomial")
  predictor <- predict(m1)
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
  coord_cartesian(ylim=c(.8,1)) +
  ggtitle("AUC Of Trivariate GLMER QAP Model") + 
  xlab("QAP Variables") +
  ylab("AUC")


# Now test the final model selected in the validation data set 
#finalModel <- as.formula(paste("value ~", aucValsTri$V2[1], "+ (1|variable)"))
finalModel <- as.formula("value ~ bg.kurtosis+bg.skewness+ (1|variable)")

# Now build this model on the training data
m1 <- glmer(finalModel, data=raw.lme.data, family="binomial")

# Now build an roc curve
predictor <- predict(m1)
outcome <- raw.lme.data$value
roc.final <- rocplot.single(outcome, predictor, title="bg.kurtosis + bg.skewness ROC Curve")

# Now plot them all together
pdf('stepWiseModelBuilding.pdf', width=20, height=20)
multiplot(aucZerovsNotZeroMonovariate,aucZerovsNotZeroTrivaraite,aucZerovsNotZeroBivariate,roc.final, cols=2) 
dev.off()

# Quick significane explore
model1 <- as.formula("value ~ bg.kurtosis+bg.skewness+ (1|variable)")
model2 <- finalModel
m1 <- glmer(model1, data=raw.lme.data, family="binomial")
m2 <- glmer(model2, data=raw.lme.data, family="binomial")
predictor1 <- predict(m1, newdata=raw.lme.data, allow.new.levels=T, type='response')
predictor2 <- predict(m2, newdata=raw.lme.data, allow.new.levels=T)
outcome <- raw.lme.data$value
roc.1 <- roc(outcome ~ predictor1)
roc.2 <- roc(outcome ~ predictor2)
roc.test(roc.1, roc.2)



# Now do everything in the Go1 
# Now build an roc curve
validationData$variable <- rep('ratingNULL', nrow(validationData))
predictor <- predict(m1, newdata=validationData, allow.new.levels=T, type='response')
outcome <- validationData$averageRating.x
roc.go1.valid <- rocplot.single(outcome, predictor, title="bg.kurtosis + bg.skewness ROC Curve Go1 Valid")

# Now do everything in the mgi data
# First load the data
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$variable <- rep("ratingNULL", nrow(raw.lme.data))
predictor <- predict(m1, newdata=raw.lme.data, allow.new.levels=T)
outcome <- raw.lme.data$averageRating.x
roc.mgi.valid <- rocplot.single(outcome, predictor, title="bg.kurtosis + bg.skewness ROC Curve MGI Valid")

# Now plot all of our roc curves
pdf('finalAndValidROCCruves.pdf', width=18, height=12)
multiplot(roc.final, roc.go1.valid, roc.mgi.valid, cols=3)
dev.off()
