# AFGR June 8 2016
# This script is going to be used to produce table 2 for the qap paper
# It is going to produce a table with the mean CV'ed weights from the models
# Pretty much the same process as figure5 but just different output

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
outputWeights <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
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
  w <- abs(unname(fixef(m1)[-1]))
  w <- exp(w) 
}
stopCluster(cl)

cols <- c('CNR', 'EFC', 'FBER', 'FWHM',
                            'QI1', 'SNR', 'CSF Kurtosis', 
                            'CSF Skewness', 'GM Kurtosis',
                            'GM Skewness', 'WM Kurtosis',
                            'WM Skewness', 'BG Kurtosis',
                            'BG Skewness') 


colnames(outputWeights) <- cols
go1.data <- apply(outputWeights,2,mean)
go1.data <- go1.data[order(names(go1.data))]

## Now do the MGI data
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
outputWeights <- foreach(i=seq(1,length(folds)), .combine='rbind') %dopar% {
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
  w <- abs(unname(fixef(m1)[-1]))
  w <- exp(w)  
}
stopCluster(cl)


colnames(outputWeights) <- cols
mgi.data <- apply(outputWeights,2,mean)
mgi.data <- mgi.data[order(names(mgi.data))]


output <- cbind(go1.data, mgi.data)
write.csv(output, 'table2QapPaper.csv', row.names=T, quote=F)
