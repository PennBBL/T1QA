# AFGR July 25 2016
# I am going to perform the 0 vs !0 and the 1 vs 2 class classifaction in this script
# There are sevreal things I have to perform extremally carefully within this script though
# These include:
#	1.) Creating the train and valid models before melt the data
#	2.) predict w/o a mixed effect 
#	3.) scale the data before I melt it 
#	4.) find the AUC on the model of 0 vs !0 -> rm the known 0's 
# I am also doing a lot of prep for the Taki meeting in this script
# I will have to come back to make note of all that I did in this script to prepare for that later



## First thing I need to do is load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Load Library(s)
install_load('psych', 'caret','pROC', 'lme4', 'reshape2', 'foreach', 'doParallel', 'visreg', 'ggplot2', 'scales')

# Set seed
set.seed(18)

# Now I need to create the training and validation sets 
mergedQAP$tmp <- mergedQAP$averageRating
mergedQAP$tmp[mergedQAP$tmp >=1] <- 1
folds <- createFolds(mergedQAP$tmp, k=5, list=T, returnTrain=T)


# Declare the model
model <- as.formula(value ~  
               efc + fber + fwhm + 
                snr  + 
               wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now run the CV
# Now run this sucker in parallel
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputPredictedValues <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
  library('lme4')
  library('pROC')
  library('reshape2')
  set.seed(16)
  index <- as.numeric(unlist(folds[i]))
  raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
  raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
  validate <- raw.lme.data[index,]
  train <- raw.lme.data[-index,]
  raw.lme.data.train <- melt(train, id.vars=names(train)[1:32], measure.vars=names(train)[34:36])
  raw.lme.data.train$value[raw.lme.data.train$value==2] <- 1
  m1 <- glmer(model, data=raw.lme.data.train, family='binomial', 
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))
  validate$variable <- rep('ratingNULL', nrow(validate))
  outcome <- as.vector(predict(m1, newdata=validate, type='response', allow.new.levels=TRUE))
  response <- as.numeric(as.character(validate$averageRating.x))
  response[response>1] <- 1 
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
  #raw.lme.data.tmp <- raw.lme.data
  #raw.lme.data.tmp$variable <- rep('ratingNULL', nrow(raw.lme.data.tmp))
  #as.vector(predict(m1, newdata=raw.lme.data.tmp, type='response', allow.new.levels=TRUE))
}
stopCluster(cl)

## Now I need to find the average cut off for the roc curve
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputRocCutoff <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
  library('lme4')
  library('pROC')
  library('reshape2')
  set.seed(16)
  index <- as.numeric(unlist(folds[i]))
  raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
  raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
  validate <- raw.lme.data[-index,]
  train <- raw.lme.data[index,]
  raw.lme.data.train <- melt(train, id.vars=names(train)[1:32], measure.vars=names(train)[34:36])
  raw.lme.data.train$value[raw.lme.data.train$value==2] <- 1
  m1 <- glmer(model, data=raw.lme.data.train, family='binomial', 
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))
  validate$variable <- rep('ratingNULL', nrow(validate))
  outcome <- as.vector(predict(m1, newdata=validate, type='response', allow.new.levels=TRUE))
  response <- as.numeric(as.character(validate$averageRating.x))
  response[response>1] <- 1 
  roc.tmp <- roc(response ~ outcome)
  unname(coords(roc.tmp, 'best')[1])
}
stopCluster(cl)


## Now I want to find the relationship between my predicted bad images and their cortical thickness values 
## To do this I want to find the predicted quality class by training a model on all of my data
## and then predicitng on the entire data set w/o the rater class
## First thing I need to do is create a data set in long format 
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data <- melt(raw.lme.data, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
## Now I want to train the model
m1 <- glmer(model, data=raw.lme.data, family='binomial', 
      control=glmerControl(optimizer="bobyqa", 
             optCtrl = list(maxfun = 1000000000)))

## Now I want to predict the output
raw.lme.data.1 <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data.1[,2:32] <- scale(raw.lme.data.1[,2:32], center=T, scale=T)
raw.lme.data.1$variable <- rep('ratingNULL', nrow(raw.lme.data.1))
outcome <- as.vector(predict(m1, newdata=raw.lme.data.1, type='response', allow.new.levels=TRUE))
response <- as.numeric(as.character(raw.lme.data.1$averageRating.x))
response[response>1] <- 1 
roc.tmp <- roc(response ~ outcome)
# Now find the cut off for the predicted values
cutoffToApply <- unname(coords(roc.tmp, 'best')[1])

# Now find which images are predicted to be 0 images
bblid.list <- raw.lme.data.1$bblid[which(outcome < cutoffToApply)]

# Now comapre the cortical thickness values for the flagged images for non flagged images
mergedQAP$flaggedList <- rep(1, nrow(mergedQAP))
mergedQAP$flaggedList[match(bblid.list, mergedQAP$bblid.x)] <- 0

# Now plot the difference in cortical thickness
ctVals0vsnot0 <- summarySE(data=mergedQAP, groupvars='flaggedList', measurevar='mprage_fs_mean_thickness', na.rm=T)

barPlotToPrint <- ggplot(ctVals0vsnot0, aes(x=factor(flaggedList), y=mprage_fs_mean_thickness, fill=factor(flaggedList))) + 
                    geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                    geom_errorbar(aes(ymin=mprage_fs_mean_thickness-se, ymax=mprage_fs_mean_thickness+se),
                    width= .2, position=position_dodge(.9)) + 
                    ggtitle("Pred. Unusable Images CT vs Pred. Usable Images CT") + 
                    xlab("Prediced Class") +
                    ylab("Mean FS Cortical Thickness")+ 
                    scale_y_continuous(limits=c(2.5, 2.8),oob=rescale_none)

fit1 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + flaggedList, data=mergedQAP)
visreg(fit1, "ageAtGo1Scan", by="flaggedList", overlay=TRUE)


ctVals0vsnot0True <- summarySE(data=mergedQAP, groupvars='tmp', measurevar='mprage_fs_mean_thickness', na.rm=T)

barPlotToPrintTrue <- ggplot(ctVals0vsnot0True, aes(x=factor(tmp), y=mprage_fs_mean_thickness, fill=factor(tmp))) + 
                    geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                    geom_errorbar(aes(ymin=mprage_fs_mean_thickness-se, ymax=mprage_fs_mean_thickness+se),
                    width= .2, position=position_dodge(.9)) + 
                    ggtitle("Actual Unusable Images CT vs Actual Usable Images CT") + 
                    xlab("Actual Class") +
                    ylab("Mean FS Cortical Thickness")+ 
                    scale_y_continuous(limits=c(2.5, 2.8),oob=rescale_none)

fit2 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + tmp, data=mergedQAP)
visreg(fit2, "ageAtGo1Scan", by="tmp", overlay=TRUE)
mergedQAP$averageRating <- as.factor(mergedQAP$averageRating)
fit3 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + averageRating, data=mergedQAP)
visreg(fit3, "ageAtGo1Scan", by="averageRating", overlay=TRUE)

pdf("imagesForTakiMeeting0vsNot0.pdf")
print(barPlotToPrint)
print(visreg(fit1, "ageAtGo1Scan", by="flaggedList", overlay=TRUE))
print(barPlotToPrintTrue)
print(visreg(fit2, "ageAtGo1Scan", by="tmp", overlay=TRUE))
print(visreg(fit3, "ageAtGo1Scan", by="averageRating", overlay=TRUE, alpha=1))
dev.off()


# Now model the linear regression
# Declare the model
model <- as.formula(value ~  
               efc + fber + fwhm + 
                snr  + 
               wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data <- melt(raw.lme.data, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
m <- lmer(model, data=raw.lme.data)

# Now predict the values
raw.lme.data.1 <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data.1[,2:32] <- scale(raw.lme.data.1[,2:32], center=T, scale=T)
raw.lme.data.1$variable <- rep('ratingNULL', nrow(raw.lme.data.1))
outcome <- as.vector(predict(m, newdata=raw.lme.data.1, type='response', allow.new.levels=TRUE))
pdf('lmerCOntinousVariableExample.pdf')
plot(raw.lme.data.1$rawAverageRating, outcome, xlim=c(0,2), ylim=c(-1,3), ylab="Actual Average Rating",
     xlab="LMER Model Outcome", main="LMER Outcome values vs Raw Average Rating")
abline(0,1)
legend(x='bottomright', legend=paste('Cor =' ,round(cor(raw.lme.data.1$rawAverageRating, outcome),2)))
dev.off()

raw.lme.data.1 <- cbind(raw.lme.data.1, outcome)
tmp <- merge(raw.lme.data.1, mergedQAP, by.x='bblid', by.y='bblid.x')
pdf('lmerVsCT.pdf')
plot(tmp$outcome, tmp$mprage_fs_mean_thickness, ylab="Mean FS CT", xlab="LMER Model Outcomes")
legend(x='bottomright', legend=paste('Cor =' ,round(cor(tmp$mprage_fs_mean_thickness, tmp$outcome, use="complete"),2)))
dev.off()



# Now find the AUC when we train on 1 vs 2 data
mergedQAP.step.two <- mergedQAP[which(mergedQAP$averageRating!=0),]
isolatedVars <- isolatedVars[which(isolatedVars$averageRating!=0),]
manualQAData2 <- manualQAData2[which(manualQAData2$averageRating!=0),]
mergedQAP.step.two$averageRating <- as.numeric(as.character(mergedQAP.step.two$averageRating))
mergedQAP.step.two$averageRating[mergedQAP.step.two$averageRating<1.4] <- 0
mergedQAP.step.two$averageRating[mergedQAP.step.two$averageRating>1.5] <- 1
# Now decalre the folds 
folds <- createFolds(mergedQAP.step.two$averageRating, k=5, list=T, returnTrain=T)

# Now create the models and validation models
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
  library('lme4')
  library('pROC')
  library('reshape2')
  set.seed(16)
  index <- as.numeric(unlist(folds[i]))
  raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
  raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
  validate <- raw.lme.data[-index,]
  train <- raw.lme.data[index,]
  raw.lme.data.train <- melt(train, id.vars=names(train)[1:32], measure.vars=names(train)[34:36])
  raw.lme.data.train$value[raw.lme.data.train$value==1] <- 0
  raw.lme.data.train$value[raw.lme.data.train$value==2] <- 1
  m1 <- glmer(model, data=raw.lme.data.train, family='binomial', 
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))
  validate$variable <- rep('ratingNULL', nrow(validate))
  outcome <- as.vector(predict(m1, newdata=validate, type='response', allow.new.levels=TRUE))
  response <- as.numeric(as.character(validate$averageRating.x))
  response[response<1.34] <- 0
  response[response>1.4] <- 1
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
}
stopCluster(cl)

# I now want to compare the cortical thickness values of the predicted outcomes 
# First find the orignal ground truth differences in cortical thickness aomnogst the different groups
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data <- melt(raw.lme.data, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value==1] <- 0
raw.lme.data$value[raw.lme.data$value==2] <- 1
m1 <- glmer(model, data=raw.lme.data, family='binomial', 
      control=glmerControl(optimizer="bobyqa", 
             optCtrl = list(maxfun = 1000000000)))


## Now I want to predict the output
raw.lme.data.1 <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data.1[,2:32] <- scale(raw.lme.data.1[,2:32], center=T, scale=T)
raw.lme.data.1$variable <- rep('ratingNULL', nrow(raw.lme.data.1))
outcome <- as.vector(predict(m1, newdata=raw.lme.data.1, type='response', allow.new.levels=TRUE))
response <- as.numeric(as.character(raw.lme.data.1$averageRating.x))
response[response<1.34] <- 0
response[response>1.34] <- 1
roc.tmp <- roc(response ~ outcome)
# Now find the cut off for the predicted values
cutoffToApply <- unname(coords(roc.tmp, 'best')[1])

# Now find which images are predicted to be 0 images
bblid.list <- raw.lme.data.1$bblid[which(outcome < cutoffToApply)]

# Now comapre the cortical thickness values for the flagged images for non flagged images
mergedQAP.step.two$flaggedList <- rep(1, nrow(mergedQAP.step.two))
mergedQAP.step.two$flaggedList[match(bblid.list, mergedQAP.step.two$bblid.x)] <- 0
# Now plot the difference in cortical thickness
ctVals1vs2 <- summarySE(data=mergedQAP.step.two, groupvars='flaggedList', measurevar='mprage_fs_mean_thickness', na.rm=T)

barPlotToPrint <- ggplot(ctVals1vs2, aes(x=factor(flaggedList), y=mprage_fs_mean_thickness, fill=factor(flaggedList))) + 
                    geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                    geom_errorbar(aes(ymin=mprage_fs_mean_thickness-se, ymax=mprage_fs_mean_thickness+se),
                    width= .2, position=position_dodge(.9)) + 
                    ggtitle("Pred. Unusable Images CT vs Pred. Usable Images CT") + 
                    xlab("Prediced Class") +
                    ylab("Mean FS Cortical Thickness") + 
                    scale_y_continuous(limits=c(2.5, 2.8),oob=rescale_none)

fit1 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + flaggedList, data=mergedQAP.step.two)
visreg(fit1, "ageAtGo1Scan", by="flaggedList", overlay=TRUE)

ctVals1vs2True <- summarySE(data=mergedQAP.step.two, groupvars='averageRating', measurevar='mprage_fs_mean_thickness', na.rm=T)
barPlotToPrintTrue <- ggplot(ctVals1vs2True, aes(x=factor(averageRating), y=mprage_fs_mean_thickness, fill=factor(averageRating))) + 
                    geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                    geom_errorbar(aes(ymin=mprage_fs_mean_thickness-se, ymax=mprage_fs_mean_thickness+se),
                    width= .2, position=position_dodge(.9)) + 
                    ggtitle("Actual Unusable Images CT vs Actual Usable Images CT") + 
                    xlab("Actual Class") +
                    ylab("Mean FS Cortical Thickness")+ 
                    scale_y_continuous(limits=c(2.5, 2.8),oob=rescale_none)

fit2 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + averageRating, data=mergedQAP.step.two)
visreg(fit2, "ageAtGo1Scan", by="averageRating", overlay=TRUE)
pdf('1vs2Compare.pdf')
print(barPlotToPrint)
print(visreg(fit1, "ageAtGo1Scan", by="flaggedList", overlay=TRUE))
print(barPlotToPrintTrue)
print(visreg(fit2, "ageAtGo1Scan", by="averageRating", overlay=TRUE))
dev.off()


# Now do the same thing with the lavaan package
library(lavaan)
Y <- mergedQAP.step.two$mprage_fs_mean_thickness
X <- mergedQAP.step.two$ageAtGo1Scan
M <- mergedQAP.step.two$averageRating
Data <- data.frame(X = X, Y=Y, M=M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = Data, se="bootstrap", bootstrap=1000)



###
###
###
###
##
## Things I need to do before the taki meeting
##
##
# Check to see how the predicted average values classes differ amongst cortical thickness
# Find the average upper left hand point amongst the roc curve and use that as the cutt off point for each of my to determine groups 
# Plot the vis reg for this with and without an interaction amongst the age and outcome grpup
# I also need to provide an explanation for why we are moving away from a continous variable for the outcome 
# between cortical thickness and a quality variable ( using the previously created composite qap variable)
# Also need to look into to see how the trichotoms logistic regression works 





