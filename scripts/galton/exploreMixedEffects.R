# AFGR April 26 2016 

# This script is going to be used to explore a couple of more qap analysis options included will be:
#	1.) using a lme to measure IRR both inter and intra???
#	2.) principal compontents analysis on the qap data
#	3.) mixed effects with each rater as a random effect -- not sure how to do this
#	4.) SVM with group weights to avoid classifying way to many bad images


## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'plyr', 'visreg', 'scales', 'stats', 'lme4','reshape2')

## Declare some functions


## Load the data
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n1601_qap_output.csv")
#kurtVals <- read.csv("/home/adrose/qapQA/data/allTissueSkewAndKurtVals.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n1601_skew_kurt_values.csv")
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n1601_manual_ratings.csv")

# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
attach(manualQAData2)
rawAverageRating <- as.data.frame(cbind(bblid, averageRating))
colnames(rawAverageRating)[2] <- 'rawAverageRating'
detach(manualQAData2)
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
manualQAData2$averageRating[which(manualQAData2$averageRating>.99)] <- 1
manualQAData2 <- merge(manualQAData2, rawAverageRating, by='bblid')

manualQAData <- merge(manualQAData, manualQAData2, by='bblid')
qapRawOutput <- merge(qapRawOutput,kurtVals ,by.x="subject", by.y="bblid")

# add bblid and scan id columns to qapRawOutput variable 
# prime a NA value bblid column
naCol <- rep(NA, length(qapRawOutput$subject))
qapRawOutput$bblid <- naCol
qapRawOutput$scanid <- naCol

# Now go through each subject id and split the string and reutnr just the first value of that strsplit
for (subjectIndex in 1:length(qapRawOutput$subject)){
  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][1]
  qapRawOutput$bblid[subjectIndex] <- stringSplitOutput
  stringSplitOutput <- strsplit(as.character(qapRawOutput$subject[subjectIndex]), split="_")[[1]][2]
  qapRawOutput$scanid[subjectIndex] <- stringSplitOutput
}

# Now turn them back into factors 
qapRawOutput$bblid <- as.factor(qapRawOutput$bblid)
qapRawOutput$scanid <- as.factor(qapRawOutput$scanid)

# Now merge the data 
mergedQAP <- merge(qapRawOutput, manualQAData, by="scanid")
mergedQAP <- mergedQAP[!duplicated(mergedQAP),]

## Declare some variables
manualQAValue <- "averageRating"

manualQAColVal <- grep(manualQAValue, names(mergedQAP))

qapValNames <- names(mergedQAP)[5:40]

## Now prep some derivative data sets
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid.x, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
goodvsBadData <- isolatedVars[which(isolatedVars[manualQAValue]==0),]
goodvsBadData <- rbind(goodvsBadData, isolatedVars[which(isolatedVars[manualQAValue]==2),])
goodvsBadData$averageRating[which(goodvsBadData$averageRating==2)] <- 1
goodvsBadData$averageRating <- factor(goodvsBadData$averageRating)

## Now I want to run the lmer for rating(0 vs !0) ~ (1|rater) + rating + first 12 pca compontents
## The input data will have 16?? columns these include:
## 1.) bblid 2.) rating 3.) rater 4-16.) Compontnets from PCA
## First rm the size variables from the isolated variables df
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
components.pca <- prcomp(tmp[,2:30], scale.=T, center=T)
components <- components.pca$x[,1:12]
#lme.data <- melt(tmp, id.vars=c('bblid', 'ratingKS', 'ratingJB', 'ratingLV'),
#                 measure.vars=c('bg_mean','bg_size','bg_std','cnr','csf_mean',
#                 'csf_size','csf_std','efc','fber','fg_mean','fg_size','fg_std',
#                 'fwhm','fwhm_x','fwhm_y','fwhm_z','gm_mean','gm_size','gm_std',
#                 'qi1','snr','wm_mean','wm_size','wm_std','all.kurtosis','all.skewness',
#                 'csf.kurtosis','csf.skewness','gm.kurtosis','gm.skewness','wm.kurtosis',
#                 'bg.skewness','bg.kurtosis'))
tmp <- cbind(tmp, components)

cols.of.interest <- c(1,33,34,35,36,39,40,41,42,43,44,45,46,47,48,49,50)
lme.data <- tmp[,cols.of.interest]
lme.data <- melt(lme.data, id.vars=c('bblid', 'ratingKS', 'ratingJB','ratingLV','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'))
lme.data <- melt(lme.data, id.vars=c('bblid','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'), 
                  measure.vars=c('ratingJB', 'ratingLV', 'ratingKS'))
colnames(lme.data)[2] <- 'binaryOutcome'
lme.data$binaryOutcome <- lme.data$value
lme.data$binaryOutcome[lme.data$binaryOutcome==2] <- 1
lme.data$binaryOutcome <- as.factor(as.numeric(as.character(lme.data$binaryOutcome)))

lmm.1 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data, family='binomial',control=glmerControl(optimizer="bobyqa"))
#sws <- weights(lmm.1)
#lmm.2 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data, family='binomial', weights=sws)

# Produce roc curves
response <- lme.data$binaryOutcome
outcome <- as.vector(predict(lmm.1, newdata=lme.data ,type='response'))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer 12 PC ROC curve No Size Variables', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))

## Now try to find a continous variable which can be used to regress CT
## We are going to try and do this by using the a new logistic
## regression model but this time use it for the goods and stellar images
## Hopefully this will result in some kind of corellation 
lme.data.regress <- lme.data[which(lme.data$value!=0),]
lme.data.regress$binaryOutcome <- lme.data.regress$value
lme.data.regress$binaryOutcome[lme.data.regress$binaryOutcome==1] <- 0
lme.data.regress$binaryOutcome[lme.data.regress$binaryOutcome==2] <- 1
lmm.2 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data.regress, family='binomial',control=glmerControl(optimizer="bobyqa"))
regress.values <- as.vector(predict(lmm.1))
## Rregress values now exists as the predicted output from lmm.2
## We now want to probe this values relationship with cortical thickness 
## We will start this by looking at the correllation
## First we will split the regress data 
first.bblid.index <- which(lme.data.regress$bblid==100031)
regress.values.1 <- as.data.frame(cbind(as.character(lme.data.regress$bblid[1:first.bblid.index[2]-1]), as.numeric(as.character(regress.values[1:first.bblid.index[2]-1]))))
regress.values.2 <- as.data.frame(cbind(as.character(lme.data.regress$bblid[first.bblid.index[2]:first.bblid.index[3]-1]), as.numeric(as.character(regress.values[first.bblid.index[2]:first.bblid.index[3]-1]))))
regress.values.3 <- as.data.frame(cbind(as.character(lme.data.regress$bblid[first.bblid.index[3]:length(regress.values)]), as.numeric(as.character(regress.values[first.bblid.index[3]:length(regress.values)]))))

## Now I am going to build a logistic model on raw data
## I am going to use all non size variables and perform the same 
## glmer using 1|variable that I do in the previous ones
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
lmm.4 <- glmer(value ~ bg_mean + bg_std + cnr + csf_mean + csf_std +
               efc + fber + fg_mean + fg_std + fwhm + fwhm_x + fwhm_y + fwhm_z +
               gm_mean + gm_std + qi1 + snr + wm_mean + wm_std + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable), data=raw.lme.data, 
               family='binomial',control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 10000000)))
response <- raw.lme.data$value
outcome <- as.vector(predict(lmm.4, newdata=raw.lme.data ,type='response'))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer Raw QAP Values Go1', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))




## Now we are going to find the cor for each of the objects I just created
colnames(mergedQAP) <- gsub(pattern='.x', replacement = '', x = colnames(mergedQAP), fixed = TRUE)
tmp <- merge(regress.values.1, mergedQAP, by.x='V1', by.y='bblid')
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
cor(tmp$mprage_fs_mean_thickness, as.numeric(as.character(tmp$V2)))
tmp <- merge(regress.values.2, mergedQAP, by.x='V1', by.y='bblid')
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
cor(tmp$mprage_fs_mean_thickness, as.numeric(as.character(tmp$V2)))
tmp <- merge(regress.values.3, mergedQAP, by.x='V1', by.y='bblid')
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
cor(tmp$mprage_fs_mean_thickness, as.numeric(as.character(tmp$V2)))


#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
# We have to source caret for this
install_load('caret', 'foreach', 'doParallel')
# Also now set a seed so all of this replicates
set.seed(16)
# First prepare a data set for a 0 vs !0 raw QAP analysis
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

# Now lets decalre the mdoel we are going to use 
model <- as.formula(value ~ bg_mean + bg_std + cnr + csf_mean + csf_std +
               efc + fber + fg_mean + fg_std + fwhm + fwhm_x + fwhm_y + fwhm_z +
               gm_mean + gm_std + qi1 + snr + wm_mean + wm_std + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now lets declare the all data raw qap value model
model <- as.formula(value ~ cnr + 
               efc + fber + fwhm + fwhm_x + fwhm_y + fwhm_z +
                qi1 + snr + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now lets create our folds 
folds <- createFolds(raw.lme.data$value, k=5, list=T, returnTrain=T)

# Now lets loop through each fold and build a model and validate on the left out sample
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  library('lme4')
  library('pROC')
  set.seed(16)
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]
  m1 <- glmer(model, data=train, family='binomial', 
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))
  outcome <- as.vector(predict(m1, newdata=validate, type='response'))
  response <- validate$value  
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
  # Now save the output
  file.name <- paste('allGo1QapRawValues-', i, '.RData', sep='')
  save(m1, file=file.name)
  file.name <- paste('allGo1QapRawValues-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer Raw QAP Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)

## Now I need to perform the cross validation with prinicpal components analysis 
# For this I am going to run PCA on each individual fold, so for each model I build,
# there will be 2 pca's run, one on the validation and one on the training set
model <- as.formula(value ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 
         + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable))
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucsPca <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  # Set any globals
  set.seed(16)
  library('pROC')
  library('stats')
  library('lme4')
  # Now prep the data 
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]

  # Now prepare the compoenents
  components.validate.pca <- prcomp(validate[,2:30], scale.=T, center=T)
  components.validate <- components.validate.pca$x[,1:12]
  # Now do the train
  components.train.pca <- prcomp(train[,2:30], scale.=T, center=T)
  components.train <- components.train.pca$x[,1:12]

  ## Now we have to combine the components to the data sets
  validate <- cbind(validate, components.validate)
  train <- cbind(train, components.train)

  ## Now build the model
  m2 <- glmer(model, data=train, family='binomial',
              control=glmerControl(optimizer='bobyqa',
              optCtrl = list(maxfun = 1000000000)))

  ## And now validate the model
  outcome <- as.vector(predict(m2, newdata=validate, type='response'))
  response <- validate$value
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
  
  # Now save the output
  file.name <- paste('allGo1PCA12Comp-', i, '.RData', sep='')
  save(m2, file=file.name)
  file.name <- paste('allGo1PCA12Comp-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer PCA 12 Comp Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)




## Now perform the cross validation after variable selection
## First thing I need to do within each fold is tune the alpha parameter 
## I am going to do this following kosha's methods
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucsGlm <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  # Declare any globals
  library('glmnet')
  library('pROC')
  library('lme4')
  set.seed(16)

  # Now prepare the data sets 
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]

  # Now prepare any variables for the glmnet tuning
  count<-1
  enet.alphas<-seq(0,1,by=0.05)
  enet.optparam<-matrix(nrow=21,ncol=3)
  colnames(enet.optparam)<-c("Alpha","Lambda","CVM")

  # Now run through the tuning parameter process
  for(a in enet.alphas){
    enet.alphas.cv <- cv.glmnet(data.matrix(train)[,2:32], 
                   y = train$value, family='binomial', alpha=a)
    enet.optparam[count,] <- c(a, enet.alphas.cv$lambda.min,
                       enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
    count<-count+1
  }
  
  # Now find the optimal values
  optval <- enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]
  mod <- glmnet(data.matrix(train)[,2:32], y = train$value, 
                family='binomial', alpha=optval[1], lambda=optval[2])
  model <- as.formula(paste('value ~ (1|variable) +', 
           paste(names(which(mod$beta[,1]!='0')), collapse="+"))) 
  m3 <- glmer(model, data=train, family='binomial',
        control=glmerControl(optimizer="bobyqa", 
        optCtrl = list(maxfun = 10000000))) 
  outcome <- as.vector(predict(m3, newdata=validate, type='response'))
  response <- validate$value 
  detach("package:glmnet", unload=TRUE)
  roc.tmp <- roc(response ~ outcome)
  model 

  # Now save the output
  file.name <- paste('allGo1GLMQapRaw-', i, '.RData', sep='')
  save(m3, file=file.name)
  file.name <- paste('allGo1GLMQapRaw-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer ENet QAP Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
}
stopCluster(cl)


## Now test the out of sample validation data set's
## I am basically going to go through a for loop, load each model
## and then test the validation across the whole sample
## I am not sure if this is correct - I should discuss this with Ted sometime today


## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allMgiQapRawValues-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m1, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
}


## Now do the PCA
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
components.pca <- prcomp(tmp[,2:30], scale.=T, center=T)
components <- components.pca$x[,1:12]
tmp <- cbind(tmp, components)
tmp <- cbind(tmp, components)

cols.of.interest <- c(1,33,34,35,36,39,40,41,42,43,44,45,46,47,48,49,50)
lme.data <- tmp[,cols.of.interest]
lme.data <- melt(lme.data, id.vars=c('bblid', 'ratingKS', 'ratingJB','ratingLV','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'))
lme.data <- melt(lme.data, id.vars=c('bblid','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'), 
                  measure.vars=c('ratingJB', 'ratingLV', 'ratingKS'))
colnames(lme.data)[2] <- 'binaryOutcome'
lme.data$binaryOutcome <- lme.data$value
lme.data$binaryOutcome[lme.data$binaryOutcome==2] <- 1
lme.data$value <- as.factor(as.numeric(as.character(lme.data$binaryOutcome)))
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allMgiPCA12Comp-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m2, newdata=lme.data, type='response'))
  response <- lme.data$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
}


## Now do the ENet output
## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
for(i in seq(2,3,1)){
  file.name <- paste('allMgiGLMQapRaw-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m1, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
}







##################
##################
##################
#####1 vs !1 #####
##################
##################
##################
# This is the most pivotal part of the analysis I am going to go through
# and use each previous model creation method and try to 
# find the best model method to regress data quality with
# I am how ever going to start with the pca 
# because that is the one I am most optimistic about 

## First thing I am going to do is prepare the CV'ed auc's and out of smaple AUCs for the 1 vs 2 models

# Also now set a seed so all of this replicates
set.seed(16)
# First prepare a data set for a 0 vs !0 raw QAP analysis
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
raw.lme.data$value[raw.lme.data$value==1] <- 0
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
# Now lets declare the all data raw qap value model
model <- as.formula(value ~ cnr + 
               efc + fber + fwhm + fwhm_x + fwhm_y + fwhm_z +
                qi1 + snr + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now lets create our folds 
folds <- createFolds(raw.lme.data$value, k=10, list=T, returnTrain=T)

# Now lets loop through each fold and build a model and validate on the left out sample
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  library('lme4')
  library('pROC')
  set.seed(16)
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]
  m1 <- glmer(model, data=train, family='binomial', 
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))
  outcome <- as.vector(predict(m1, newdata=validate, type='response'))
  response <- validate$value  
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
  # Now save the output
  file.name <- paste('allGo1QapRawValues1vs2-', i, '.RData', sep='')
  save(m1, file=file.name)
  file.name <- paste('allGo1QapRawValues-ROC-Cruve1vs2', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer Raw QAP Values Go1 1vs2', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)




####
#### Now do the PCA
model <- as.formula(value ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 
         + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable))
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucsPca <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  # Set any globals
  set.seed(16)
  library('pROC')
  library('stats')
  library('lme4')
  # Now prep the data 
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]

  # Now prepare the compoenents
  components.validate.pca <- prcomp(validate[,2:32], scale.=T, center=T)
  components.validate <- components.validate.pca$x[,1:12]
  # Now do the train
  components.train.pca <- prcomp(train[,2:32], scale.=T, center=T)
  components.train <- components.train.pca$x[,1:12]

  ## Now we have to combine the components to the data sets
  validate <- cbind(validate, components.validate)
  train <- cbind(train, components.train)

  ## Now build the model
  m2 <- glmer(model, data=train, family='binomial',
              control=glmerControl(optimizer='bobyqa',
              optCtrl = list(maxfun = 1000000000)))

  ## And now validate the model
  outcome <- as.vector(predict(m2, newdata=validate, type='response'))
  response <- validate$value
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)
  
  # Now save the output
  file.name <- paste('allGo1PCA12Comp1vs2-', i, '.RData', sep='')
  save(m2, file=file.name)
  file.name <- paste('allGo1PCA12Comp-ROC-Cruve1vs2', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer PCA 12 Comp Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)


## Now perform the cross validation after variable selection
## First thing I need to do within each fold is tune the alpha parameter 
## I am going to do this following kosha's methods
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucsGlm <- foreach(i=seq(1,length(folds)), .combine='c') %dopar% {
  # Declare any globals
  library('glmnet')
  library('pROC')
  library('lme4')
  set.seed(16)

  # Now prepare the data sets 
  index <- as.numeric(unlist(folds[i]))
  train <- raw.lme.data[index,]
  validate <- raw.lme.data[-index,]

  # Now prepare any variables for the glmnet tuning
  count<-1
  enet.alphas<-seq(0,1,by=0.05)
  enet.optparam<-matrix(nrow=21,ncol=3)
  colnames(enet.optparam)<-c("Alpha","Lambda","CVM")

  # Now run through the tuning parameter process
  for(a in enet.alphas){
    enet.alphas.cv <- cv.glmnet(data.matrix(train)[,2:32], 
                   y = train$value, family='binomial', alpha=a)
    enet.optparam[count,] <- c(a, enet.alphas.cv$lambda.min,
                       enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
    count<-count+1
  }
  
  # Now find the optimal values
  optval <- enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]
  mod <- glmnet(data.matrix(train)[,2:32], y = train$value, 
                family='binomial', alpha=optval[1], lambda=optval[2])
  model <- as.formula(paste('value ~ (1|variable) +', 
           paste(names(which(mod$beta[,1]!='0')), collapse="+"))) 
  m3 <- glmer(model, data=train, family='binomial',
        control=glmerControl(optimizer="bobyqa", 
        optCtrl = list(maxfun = 10000000))) 
  outcome <- as.vector(predict(m3, newdata=validate, type='response'))
  response <- validate$value 
  detach("package:glmnet", unload=TRUE)
  roc.tmp <- roc(response ~ outcome)

  # Now save the output
  file.name <- paste('allGo1GLMQapRaw1vs2-', i, '.RData', sep='')
  save(m3, file=file.name)
  file.name <- paste('allGo1GLMQapRaw-ROC-Cruve1vs2', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer ENet QAP Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)


## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allMgiQapRawValues1vs2-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m1, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
}


## Now do the PCA
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
components.pca <- prcomp(tmp[,2:30], scale.=T, center=T)
components <- components.pca$x[,1:12]
tmp <- cbind(tmp, components)
tmp <- cbind(tmp, components)

cols.of.interest <- c(1,33,34,35,36,39,40,41,42,43,44,45,46,47,48,49,50)
lme.data <- tmp[,cols.of.interest]
lme.data <- melt(lme.data, id.vars=c('bblid', 'ratingKS', 'ratingJB','ratingLV','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'))
lme.data <- melt(lme.data, id.vars=c('bblid','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'), 
                  measure.vars=c('ratingJB', 'ratingLV', 'ratingKS'))
colnames(lme.data)[2] <- 'binaryOutcome'
lme.data$binaryOutcome <- lme.data$value
lme.data$binaryOutcome[lme.data$binaryOutcome==2] <- 1
lme.data$value <- as.factor(as.numeric(as.character(lme.data$binaryOutcome)))
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allMgiPCA12Comp1vs2-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m2, newdata=lme.data, type='response'))
  response <- lme.data$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
}


## Now do the ENet output
## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
for(i in seq(1,5,1)){
  file.name <- paste('allMgiGLMQapRaw1vs2-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m1, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
}
















# Prepare the PCA data
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
components.pca <- prcomp(tmp[,2:30], scale.=T, center=T)
components <- components.pca$x[,1:12]
tmp <- cbind(tmp, components)
cols.of.interest <- c(1,33,34,35,36,39,40,41,42,43,44,45,46,47,48,49,50)
lme.data <- tmp[,cols.of.interest]
lme.data <- melt(lme.data, id.vars=c('bblid', 'ratingKS', 'ratingJB','ratingLV','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'))
lme.data <- melt(lme.data, id.vars=c('bblid','averageRating.x','PC1','PC2','PC3','PC4','PC5','PC6','PC7',
                                'PC8','PC9','PC10','PC11','PC12'), 
                  measure.vars=c('ratingJB', 'ratingLV', 'ratingKS'))
colnames(lme.data)[2] <- 'binaryOutcome'
lme.data$binaryOutcome <- lme.data$value
# Now remove all 0 variables as we only want to compare the 1's vs the 2's
lme.data <- lme.data[which(lme.data$value!=0),]
lme.data$binaryOutcome <- as.factor(as.numeric(as.character(lme.data$binaryOutcome)))
# Now create the model
lmm.1 <- lmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data,control=glmerControl(optimizer="bobyqa"))
response <- lme.data$binaryOutcome
outcome <- as.vector(predict(lmm.1, newdata=lme.data ,type='response'))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer 12 PC ROC curve No Size Variables', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))


# Now I need to find the partial correllation between the 
# odds ratio and the cortical thickness
# when controlling for age sex and race
tmp <- merge(lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
tmp$outcome <- outcome
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
cor(tmp$mprage_fs_mean_thickness, tmp$outcome, method='spearman')
p.cor.string <- c('mprage_fs_mean_thickness','ageAtGo1Scan','outcome', 'sex', 'race2')
pcor(p.cor.string, var(tmp[p.cor.string]))


# Now prepare the Raw QAP data
# First prepare a data set for a 0 vs !0 raw QAP analysis
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$value[raw.lme.data$value==1] <- 0
raw.lme.data$value[raw.lme.data$value==2] <- 1

# Now lets decalre the mdoel we are going to use 
lmm.2 <- lmer(value ~ cnr + 
               efc + fber + fwhm + fwhm_x + fwhm_y + fwhm_z +
                qi1 + snr + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable), data=raw.lme.data)
response <- raw.lme.data$value
outcome <- as.vector(predict(lmm.2, newdata=raw.lme.data ,type='response'))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer 12 PC ROC curve No Size Variables', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))

# Now I need to find the partial correllation between the 
# odds ratio and the cortical thickness
# when controlling for age sex and race
tmp <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
tmp$outcome <- outcome
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
cor(tmp$mprage_fs_mean_thickness, tmp$outcome, method='spearman')
p.cor.string <- c('mprage_fs_mean_thickness', 'ageAtGo1Scan', 'outcome', 'sex', 'race2')
pcor(p.cor.string, var(tmp[p.cor.string]))




## Now I am going to try and build a lmer 
## The first general model will be 
## FS-CT ~ age + sex + race + 1vs2-imagequality + (1|rater)
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$value[raw.lme.data$value==1] <- 0
raw.lme.data$value[raw.lme.data$value==2] <- 1
tmp <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
tmp$outcome <- outcome
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
lmer.data <- tmp
rm(tmp)
model <- as.formula(mprage_fs_mean_thickness ~ ageAtGo1Scan + sex + race2 + value +(1|variable))
m.1 <- lmer(model, data=lmer.data)

