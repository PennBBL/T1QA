w <- unname(fixef(m1))[-1]
cols <- names(fixef(m1))[-1]
reg.vals <- apply(raw.lme.data[cols], 1, function(x) weighted.mean(x, w))
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
tmp <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]






# Also now set a seed so all of this replicates
set.seed(16)
# First prepare a data set for a 1 vs 2 raw QAP analysis
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
raw.lme.data$value[raw.lme.data$value==1] <- 0
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)


# Now do 0 vs !0
# First prepare a data set for a 0 vs !0 raw QAP analysis
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data$value[raw.lme.data$value==2] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)





# Now lets declare the all data raw qap value model
model <- as.formula(value ~ cnr + 
               efc + fber + fwhm + 
                qi1 + snr  + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now lets create our folds 
folds <- createFolds(raw.lme.data$value, k=5, list=T, returnTrain=T)

# Now lets loop through each fold and build a model and validate on the left out sample
cl <- makeCluster(length(folds))
registerDoParallel(cl)
outputAucs <- foreach(i=seq(1,length(folds)), .combine='cbind') %dopar% {
  library('lme4')
  library('pROC')
  library('reshape2')
  library('ggm')
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
  ## Now do the p cor between composite qap val and CT
  w <- abs(unname(fixef(m1)[-1]))
  w <- exp(w) 
  cols <- names(fixef(m1))[-1]
  #cols <- colsToUse
  flip.index <- c(1, -1, 1, -1, -1, 1, -1, -1, 1, -1, -1, -1, 1, -1)
  #flip.index <- c(-1, 1, 1, -1, 1, -1)
  w <- w*flip.index
  reg.vals <- apply(raw.lme.data[cols], 1, function(x) weighted.mean(x, w))
  tmp <- merge(isolatedVars, manualQAData2, by='bblid')
  raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
  raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
  #raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
  raw.lme.data <- cbind(raw.lme.data, reg.vals)
  tmp <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
  tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
  p.cor.string <- c('mprage_fs_mean_thickness','reg.vals', 'ageAtGo1Scan','sex', 'race2')
  pcor(p.cor.string, var(tmp[p.cor.string]))
}
stopCluster(cl)


## Now find the p cor between motion metrics and raw rating 
# First create a string with all motion metrics in it
qaMetrics <- append('reg.vals',names(tmp)[grep('rms', names(tmp))])
output <- NULL
for(i in qaMetrics){
  p.cor.string <- c(paste(i),'manualRating',"ageAtGo1Scan","sex", "race2")
  tmp2 <- tmp[complete.cases(tmp[i]),]
  print(i)
  print(range(tmp2[i])[2] - range(tmp2[i])[1])
  output <- append(output, pcor(p.cor.string, var(tmp2[p.cor.string])))
}

bar.plot.p.cor.data.frame <- as.data.frame(cbind(output, qaMetrics))

ggplot(bar.plot.p.cor.data.frame, aes(x=qaMetrics, y=abs(as.numeric(as.character(output))), fill=output)) + 
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle=45,hjust=1)) +
      labs(title = "Partial Correllation of Motion and Comp QAP w/ Manual Rating", x = "QAP Measures", y = "Partial Correllation") + 
      theme(legend.position = 'none')





### Now plot the comparision between reg.vals, bg.kurtosis, motion metrics(mean rel rms) and average rms
### Do this for only !0 scans 
### In order to do this I have to set i to one to produce the reg vals
i <- 1
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
## Now do the p cor between composite qap val and CT
w <- abs(unname(fixef(m1)[-1]))
w <- exp(w) 
cols <- names(fixef(m1))[-1]
flip.index <- c(1, -1, 1, -1, -1, 1, -1, -1, 1, -1, -1, -1, 1, -1)
#flip.index <- c(-1, 1, 1, -1, 1, -1)
w <- w*flip.index
reg.vals <- apply(raw.lme.data[cols], 1, function(x) weighted.mean(x, w))
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data <- cbind(raw.lme.data, reg.vals)
raw.lme.data <- raw.lme.data[which(raw.lme.data$value!=0),]
tmp <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')
tmp <- tmp[complete.cases(tmp$mprage_fs_mean_thickness),]
tmp <- tmp[!duplicated(tmp$bblid),]
# Now create an average - mean rel rms value
meanRmsCols <- grep('Meanrelrms', names(tmp))
meanRelRms <- apply(tmp[,meanRmsCols], 1, function(x) mean(x, na.rm=T))
# Now create a simple average of the significant qap values
colnames(tmp) <- gsub(pattern='.x', replacement = '', x = colnames(tmp), fixed = TRUE)
sigVals <- names(which(summary(m1)$coefficients[,4]<.05))[-1]
meanOfSigVals <- apply(tmp[sigVals], 1, function(x) mean(x, na.rm=T))

tmp <- cbind(tmp, meanRelRms)
tmp <- cbind(tmp, meanOfSigVals)
p.cor.data <- tmp
p.cor.values <- names(tmp)[grep('Meanrelrms', names(tmp))]
p.cor.values <- append(p.cor.values, 'reg.vals')
p.cor.values <- append(p.cor.values, 'meanRelRms')
p.cor.values <- append(p.cor.values, 'bg.kurtosis')
p.cor.values <- append(p.cor.values, 'meanOfSigVals')


p.cor.vector <- NULL
for(qapMetricName in p.cor.values){
  p.cor.string <- c('mprage_fs_mean_thickness', qapMetricName, "ageAtGo1Scan", "sex", "race2")
  tmp <- p.cor.data[complete.cases(p.cor.data[qapMetricName]),]
  p.cor.val <- pcor(p.cor.string, var(tmp[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)
}
p.cor.data.frame <- as.data.frame(cbind(p.cor.values, p.cor.vector))

pdf('potentialRegressorsvsMeanCT.pdf')
ggplot(p.cor.data.frame, aes(x=p.cor.values, 
       y=as.numeric(as.character(p.cor.vector)), fill=as.numeric(as.character(p.cor.vector)))) + 
       geom_bar(stat="identity") +
       theme(axis.text.x = element_text(angle=45,hjust=1)) +
       labs(title = "P Cor  btn Potential Regressors and CT", x = "Regressors", y ="Partial Correllation") + 
       theme(legend.position = 'none')

ggplot(p.cor.data.frame, aes(x=p.cor.values, 
       y=abs(as.numeric(as.character(p.cor.vector))), fill=abs(as.numeric(as.character(p.cor.vector))))) + 
       geom_bar(stat="identity") +
       theme(axis.text.x = element_text(angle=45,hjust=1)) +
       labs(title = "ABS(P Cor)  btn Potential Regressors and CT", x = "Regressors", y ="Partial Correllation") + 
       theme(legend.position = 'none')
dev.off()
