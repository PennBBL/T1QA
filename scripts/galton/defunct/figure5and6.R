# AFGR June 8 2016
# This script is going to be used to produce figure 5 for the qap paper
# The basic format of this picture is going to be a 2x2 images of ROC curves
# 1,1 will be CV'ed Go1 ROC's w/ average
# 1,2 will be CV'ed MGI ROC's w/ average
# 2,1 will be Go1 trained MGI validated
# 2,2 will be MGI trained Go1 validated



### STart with all Go1 Images here!!!
# First things first - load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Load Library(s)
install_load('psych', 'caret', 'foreach', 'doParallel', 'pROC', 'lme4')

# Set seed
set.seed(16)

# Now prep a data set to work with
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

# Now create folds
folds <- createFolds(raw.lme.data$value, k=5, list=T, returnTrain=T)

# Create our model
model <- as.formula(value ~ cnr + efc + fber + fwhm + 
               qi1 + snr  + csf.kurtosis + csf.skewness +
               gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))


# Now run this sucker in parallel
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
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
  file.name <- paste('allGo1QapRawValues-', i, '.RData', sep='')
  save(roc.tmp, file=file.name)
  auc(roc.tmp)
}
stopCluster(cl)

# Now create an image with all of the ROC curves and an average ROC curve on it 
# STart by loading all of the individual folds and grabbing their values
load('./allGo1QapRawValues-1.RData')
sens.1 <- roc.tmp$sensitivities[-962]
spec.1 <- roc.tmp$specificities[-962]
load('./allGo1QapRawValues-2.RData')
sens.2 <- roc.tmp$sensitivities[-962]
spec.2 <- roc.tmp$specificities[-962]
load('./allGo1QapRawValues-3.RData')
sens.3 <- roc.tmp$sensitivities
spec.3 <- roc.tmp$specificities
load('./allGo1QapRawValues-4.RData')
sens.4 <- roc.tmp$sensitivities[-962]
spec.4 <- roc.tmp$specificities[-962]
load('./allGo1QapRawValues-5.RData')
sens.5 <- roc.tmp$sensitivities
spec.5 <- roc.tmp$specificities

# Now create an average sens and spec value
all.sens <- (sens.1 + sens.2 + sens.3 + sens.4 + sens.5) /5
all.spec <- (spec.1 + spec.2 + spec.3 + spec.4 + spec.5) /5
roc.tmp$sensitivities <- all.sens
roc.tmp$specificities <- all.spec
averageRoc <- auc(roc.tmp)
# ANd now lets plot all of this junk
pdf('figure5QapPaper.pdf', height=16, width=16)
par(mfrow=c(1,2))
plot(all.spec, all.sens, xlim=c(1,0), add=T, type='l', lwd=7, 
     main='Average 5 Fold CV PNC 1 ROC Curves', xlab='Specificity', ylab='Sensitivity')


load('./allGo1QapRawValues-5.RData')
plot(roc.tmp, col='orange', lwd=1.75, add=T)
load('./allGo1QapRawValues-4.RData')
plot(roc.tmp, col='yellow', lwd=1.75, add=T)
load('./allGo1QapRawValues-3.RData')
plot(roc.tmp, col='red', lwd=1.75, add=T)
load('./allGo1QapRawValues-2.RData')
plot(roc.tmp, col='green', lwd=1.75, add=T)
load('./allGo1QapRawValues-1.RData')
plot(roc.tmp, col='blue', lwd=1.75, add=T)


legend('bottomright', -1,legend=c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Average',paste('Average PNC 1 AUC = ', round(averageRoc, digits=2))),
        lwd=2.5, col=c('blue','green', 'red', 'yellow', 'orange','black','white'))

      
# Now build and train a model to validate on MGI and store the go1 data
m1.go1 <- glmer(model, data=raw.lme.data, family='binomial',
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))

go1.data <- raw.lme.data



#####
## Now do everything we just did for MGI
source('/home/adrose/qapQA/scripts/loadMgiData.R')
detachAllPackages()

# Load Library(s)
install_load('psych', 'caret', 'foreach', 'doParallel', 'pROC', 'lme4')

# Prep the mgi data to work with
# Also set a seed for replication
set.seed(16)
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

# Now declare the folds
folds <- createFolds(raw.lme.data$value, k=5, list=T, returnTrain=T)

# Now run this sucker in parallel
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
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
  save(roc.tmp, file=file.name)
  auc(roc.tmp)
}
stopCluster(cl)


# Now create the average ROC curve
load('./allMgiQapRawValues-1.RData')
sens.1 <- roc.tmp$sensitivities[-331]
spec.1 <- roc.tmp$specificities[-331]
load('./allMgiQapRawValues-2.RData')
sens.2 <- roc.tmp$sensitivities
spec.2 <- roc.tmp$specificities
load('./allMgiQapRawValues-3.RData')
sens.3 <- roc.tmp$sensitivities
spec.3 <- roc.tmp$specificities
load('./allMgiQapRawValues-4.RData')
sens.4 <- roc.tmp$sensitivities
spec.4 <- roc.tmp$specificities
load('./allMgiQapRawValues-5.RData')
sens.5 <- roc.tmp$sensitivities
spec.5 <- roc.tmp$specificities


all.sens <- (sens.1 + sens.2 + sens.3 + sens.5) /4
all.spec <- (spec.1 + spec.2 + spec.3 + spec.5) /4
averageRoc <- auc(roc.tmp)
# ANd now lets plot all of this junk
plot(all.spec, all.sens, xlim=c(1,0), add=T, type='l', lwd=7, 
     main='Average 5 Fold CV MGI ROC Curves', xlab='Specificity', ylab='Sensitivity')

load('./allMgiQapRawValues-5.RData')
plot(roc.tmp, col='orange', lwd=1.75, add=T)
load('./allMgiQapRawValues-4.RData')
plot(roc.tmp, col='yellow', lwd=1.75, add=T)
load('./allMgiQapRawValues-3.RData')
plot(roc.tmp, col='red', lwd=1.75, add=T)
load('./allMgiQapRawValues-2.RData')
plot(roc.tmp, col='green', lwd=1.75, add=T)
load('./allMgiQapRawValues-1.RData')
plot(roc.tmp, col='blue', lwd=1.75, add=T)

legend('bottomright', legend=c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Average',paste('Average MGI AUC = ', round(averageRoc, digits=2))),
        lwd=2.5, col=c('blue','green', 'red', 'yellow', 'orange','black','white'))

# Now turn off the cross validated graphs image
dev.off()


m.1.mgi <- glmer(model, data=raw.lme.data, family='binomial',
        control=glmerControl(optimizer="bobyqa", 
               optCtrl = list(maxfun = 1000000000)))

mgi.data <- raw.lme.data

# Now plot the out of sample graphs
pdf('figure6QapPaper.pdf', height=16, width=16)
par(mfrow=c(1,2))


## Now validate go1 -> mgi
outcome <- as.vector(predict(m1.go1, newdata=mgi.data, type='response'))
response <- mgi.data$value
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='Model Trained on PNC 1 Validated on MGI')
legend(x='bottomright', legend=paste('Model Trained on PNC 1 Validated on MGI AUC = ', round(auc(roc.tmp),digits=2)))


## Now validate mgi -> go1
outcome <- as.vector(predict(m.1.mgi, newdata=go1.data, type='response'))
response <- go1.data$value
roc.tmp <- roc(response ~ outcome)
plot(roc.tmp, main='Model Trained on MGI Validated on PNC 1')
legend(x='bottomright', legend=paste('Model Trained on MGI Validated on PNC 1 AUC = ', round(auc(roc.tmp),digits=2)))
dev.off()
