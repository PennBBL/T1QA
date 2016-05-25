## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'plyr', 'visreg', 'scales', 'stats', 'lme4','reshape2')

## Declare some functions
###################
## Load the data ##
###################
qapRawOutput <- read.csv("/home/adrose/qapQA/data/n920_qap_output_validation.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/n920_skew_kurt_values_validation.csv")
manualQAData <- read.csv("/home/adrose/qapQA/data/n550_mgi_demo_dx_2013-12-13.csv")
manualQAData2 <- read.csv("/home/adrose/qapQA/data/n920_manual_ratings_validation.csv")

# Here we will collapse the bins into 4 distinct groups based on average rating
# We want 4 distinct rating bins 0 - bad, 1-1.33 - decent, 1.667 - good, 2 - stellar
attach(manualQAData2)
rawAverageRating <- as.data.frame(cbind(bblid, averageRating))
colnames(rawAverageRating)[2] <- 'rawAverageRating'
detach(manualQAData2)
manualQAData2$averageRating[which(manualQAData2$averageRating<.68)] <- 0
manualQAData2$averageRating[which(manualQAData2$averageRating>.99 & manualQAData2$averageRating< 1.34)] <- 1
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

# Now create the three data sets - Go2, mgi penn, and mgi pitt
mergedQAP.pitt <- mergedQAP[which(mergedQAP$SiteID==70),]
mergedQAP.go2  <- merge(qapRawOutput, manualQAData2, by="bblid")
mergedQAP.go2 <- mergedQAP.go2[which(mergedQAP.go2$bblid %in% mergedQAP$bblid.x == 'FALSE'),]
mergedQAP.penn <- mergedQAP[which(mergedQAP$SiteID==71),]


## Declare some variables
manualQAValue <- "averageRating"

manualQAColVal <- grep(manualQAValue, names(mergedQAP))

qapValNames <- names(mergedQAP)[3:38]

## Now create a data set which only has good and bad data
mergedQAP <- rbind(mergedQAP.penn, mergedQAP.pitt)
mergedQAP <- mergedQAP[which(mergedQAP$age < 60),]
colnames(mergedQAP) <- gsub(pattern='.x', replacement = '', x = colnames(mergedQAP), fixed = TRUE)
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
goodvsBadData <- isolatedVars[which(isolatedVars[manualQAValue]==0),]
goodvsBadData <- rbind(goodvsBadData, isolatedVars[which(isolatedVars[manualQAValue]==2),])
goodvsBadData$averageRating[which(goodvsBadData$averageRating==2)] <- 1
goodvsBadData$averageRating <- factor(goodvsBadData$averageRating)

## Now I want to load the PCA results and apply the Go1 loadings 
## To the validation data set
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
#load('/home/adrose/qapQA/data/go1PCAComponents.RData')
load('/home/adrose/qapQA/data/go1PCAComponents-noSizeVars.RData')
load('../tmp5-11/foo.RData')
#load('/home/adrose/qapQA/data/go1GlmerModel.RData')
#load('/home/adrose/qapQA/data/go1GlmerModel-noSizeVars.RData')
components.pca <- prcomp(tmp[,2:32], scale.=T, center=T)
components <- predict(components.pca, tmp)[,1:12]
tmp <- cbind(tmp, components)
#cols.of.interest <- c(1,38,39,40,41,44,45,46,47,48,49,50,51,52,53,54,55)
cols.of.interest <- c(1,33,34,35,36,39,40,41,42,43,44,45,46,47,48,49,50,51)
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

#lmm.1 <- glmer(binaryOutcome ~ PC1 + PC2 +PC3 + PC4 +PC5 +PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 +(1|variable), data=lme.data, family='binomial',control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
response <- lme.data$binaryOutcome
outcome <- as.vector(predict(lmm.1, type='response', newdata=lme.data))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer 12 PC from MGI - ROC curve All MGI Under 60', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))

tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
response <- raw.lme.data$value
outcome <- as.vector(predict(lmm.4, type='response', newdata=raw.lme.data))
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='glmer Raw QAP Values Go1', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))


#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
#### Perfrom CV Down here
# We have to source caret for this
install_load('caret', 'foreach', 'doParallel')
# Also set a seed for replication
set.seed(16)
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

# Now lets declare the all data raw qap value model
model <- as.formula(value ~ cnr + 
               efc + fber + fwhm + fwhm_x + fwhm_y + fwhm_z +
                qi1 + snr + all.kurtosis + 
               all.skewness + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + hm.kurtosis + hm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# And now lets create our folds
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
  # Now save the output
  file.name <- paste('allMgiQapRawValues-', i, '.RData', sep='')
  save(m1, file=file.name)
  file.name <- paste('allMgiQapRawValues-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)


## Now do the pca analysis 
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
  file.name <- paste('allMgiPCA12Comp-', i, '.RData', sep='')
  save(m2, file=file.name)
  file.name <- paste('allMgiPCA12Comp-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer PCA 12 Comp Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()

}
stopCluster(cl)

## Now do the e net model
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
  enet.alphas<-seq(0,1,by=.25)
  enet.optparam<-matrix(nrow=5,ncol=3)
  colnames(enet.optparam)<-c("Alpha","Lambda","CVM")

  # Now run through the tuning parameter process
  for(a in enet.alphas){
    enet.alphas.cv <- cv.glmnet(data.matrix(train)[,2:30], 
                   y = train$value, family='binomial', alpha=a)
    enet.optparam[count,] <- c(a, enet.alphas.cv$lambda.min,
                       enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
    count<-count+1
  }
  
  # Now find the optimal values
  optval <- enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]
  mod <- glmnet(data.matrix(train)[,2:30], y = train$value, 
                family='binomial', alpha=optval[1], lambda=round(optval[2], digits=4),
                maxit=100000000)
  model <- as.formula(paste('value ~ (1|variable) +', 
         paste(names(which(mod$beta[,1]!='0')), collapse="+")))
  m3 <- glmer(model, data=train, family='binomial',
      control=glmerControl(optimizer="bobyqa", 
      optCtrl = list(maxfun = 1000000))) 
  outcome <- as.vector(predict(m3, newdata=validate, type='response'))
  response <- validate$value 
  detach("package:glmnet", unload=TRUE)
  roc.tmp <- roc(response ~ outcome)
  auc(roc.tmp)

  # Now save the output
  file.name <- paste('allMgiGLMQapRaw-', i, '.RData', sep='')
  save(m3, file=file.name)
  file.name <- paste('allMgiGLMQapRaw-ROC-Cruve', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer ENet QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
}
stopCluster(cl)


## Use this for loop to probe if any one individual qap value is responsible for the 
## convergence issues 
for(foo in qapValNames){
  model <- as.formula(paste('value ~ (1|variable) +', paste(foo)))
  print(model)
  m4 <- glmer(model, data=train, family='binomial',
      control=glmerControl(optimizer="bobyqa", 
      optCtrl = list(maxfun = 1000000)))
}

## Now perform the out of smaple validation 
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
for(i in seq(1,5,1)){
  file.name <- paste('allGo1QapRawValues-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m1, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
}

## Now do the PCA analysis
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
  file.name <- paste('allGo1PCA12Comp-', i, '.RData', sep='')
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


## Now do the ENet analysis 
## Now do the ENet output
## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allGo1GLMQapRaw-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m3, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
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
  file.name <- paste('allMgiQapRawValues1vs2-', i, '.RData', sep='')
  save(m1, file=file.name)
  file.name <- paste('allMgiQapRawValues-ROC-Cruve1vs2', i, '.pdf', sep='')
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
  file.name <- paste('allMgiPCA12Comp1vs2-', i, '.RData', sep='')
  save(m2, file=file.name)
  file.name <- paste('allMgiPCA12Comp-ROC-Cruve1vs2', i, '.pdf', sep='')
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
  file.name <- paste('allMgiGLMQapRaw1vs2-', i, '.RData', sep='')
  save(m3, file=file.name)
  file.name <- paste('allMgiGLMQapRaw-ROC-Cruve1vs2', i, '.pdf', sep='')
  pdf(file.name)
  plot(roc.tmp, main='glmer ENet QAP Values Go1', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  dev.off()
  auc(roc.tmp)
}
stopCluster(cl)

### Now do the validation outside of the datatset
## Now perform the out of smaple validation 
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allGo1QapRawValues1vs2-', i, '.RData', sep='')
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

## Now do the PCA analysis
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
  file.name <- paste('allGo1PCA12Comp1vs2-', i, '.RData', sep='')
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


## Now do the ENet analysis 
## Now do the ENet output
## Start with the pca models because... well I can't get the all raw validation data set to run...
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
lme.data.validate <- melt(tmp, id.vars=names(tmp)[1:37], measure.vars=names(tmp)[34:36])
lme.data.validate$value[lme.data.validate$value==2] <- 1
lme.data.validate[,2:32] <- scale(lme.data.validate[,2:32], center=T, scale=T)
auc.all <- NULL
for(i in seq(1,5,1)){
  file.name <- paste('allGo1GLMQapRaw1vs2-', i, '.RData', sep='')
  load(file.name)
  outcome <- as.vector(predict(m3, newdata=lme.data.validate, type='response'))
  response <- lme.data.validate$value
  roc.tmp <- roc(response ~ outcome)
  print(auc(roc.tmp))
  plot(roc.tmp, main='glmer Raw QAP Values Mgi', 
       print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  auc.all <- append(auc.all, auc(roc.tmp))
}

