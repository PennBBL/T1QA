# AFGR October 22 2015

# This script is going to be used to perform several tasks within the T1QA project which are listed below:
#	1.) Explain population demographics
#	2.) Explain QAP measures
#	3.) Explore relationship between demographics and QAP measures
#	4.) Explore relationship between QAP measures and manual SBIA QA


## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('corrplot', 'ggplot2', 'psych', 'e1071', 'pROC', 'ggm', 'visreg', 'MASS', 'nnet')

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
mergedQAP <- mergedQAP.pitt
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid.y, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
goodvsBadData <- isolatedVars[which(isolatedVars[manualQAValue]==0),]
goodvsBadData <- rbind(goodvsBadData, isolatedVars[which(isolatedVars[manualQAValue]==2),])
goodvsBadData$averageRating[which(goodvsBadData$averageRating==2)] <- 1
goodvsBadData$averageRating <- factor(goodvsBadData$averageRating)



###################
## Create all of ##
## the glm models##
###################

## Functions to use for this
## roc, ci, coords
## All of this logic came from DR_ROALF
## I am going to try to find which bblid are falgged by which ever parameter
## The output of this section will be a binary matrix nrow == nrow of goodvsBadData, ncol == qapVal
##  A one will indicate that this subjects qap value indiciated it as a flagged image for the associated qap value
response <- as.numeric(unlist(goodvsBadData[manualQAValue]))
outcome.done <- goodvsBadData$bblid
flip.index <- c(0,0,1,1,1,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,1)
flip.index <- c(0,0,1,1,1,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
count <- 1
pdf('roc_curve_good_vs_bad-pitt.pdf')
for(qapVal in qapValNames){
  outcome.response <- rep(0, nrow(goodvsBadData))
  outcome <- as.numeric(unlist(goodvsBadData[qapVal]))
  roc.tmp <- roc(response ~ outcome)
  plot(roc.tmp, main=qapVal, print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  output.coords.tmp <- coords(roc.tmp, 'best', ret=c('tn', 'tp', 'fp', 'fn','npv', 'ppv', 'threshold',"specificity", "sensitivity"))
  output.coords.tmp <- append(output.coords.tmp, auc(roc.tmp))
  tmp.thresh <- coords(roc.tmp,'best')[1][[1]]
  if(flip.index[count] == 1){
    bblid.index <- which(outcome < tmp.thresh)
  }
  if(flip.index[count] == 0){
    bblid.index <- which(outcome > tmp.thresh)
  }
  if(count == 1){
    output.coords <- output.coords.tmp
  }
  if(count > 1){
    output.coords <- cbind(output.coords, output.coords.tmp)
  }
  print(length(bblid.index))
  outcome.response[bblid.index] <- 1
  outcome.done <- cbind.all(outcome.done, outcome.response)
  count <- count + 1 
} 
sumAllOutcomes <- rowSums(outcome.done[,-1])
roc.tmp <- roc(response ~ sumAllOutcomes)
plot(roc.tmp, main= 'Total Flag QAP Vals', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
output.coords.tmp <- coords(roc.tmp, 'best', ret=c('tn', 'tp', 'fp', 'fn','npv', 'ppv', 'threshold',"specificity", "sensitivity"))
output.coords.tmp <- append(output.coords.tmp, auc(roc.tmp))
output.coords <- cbind(output.coords, output.coords.tmp)
dev.off()
colnames(outcome.done)[2:ncol(outcome.done)] <- qapValNames
colnames(outcome.done)[1] <- 'bblid'
colnames(output.coords)[1:36] <- qapValNames
colnames(output.coords)[37] <- 'qapValSum'

#######################
## Now plot the AUC  ##
## For all ROC Curves##
#######################
pdf('predictingGoodVsBad-penn.pdf')
dataFrameToPlot <- as.data.frame(cbind(qapValNames, output.coords[10,1:36]))
colnames(dataFrameToPlot)[2] <- 'areaUnderCurve'
colnames(dataFrameToPlot)[1] <- 'qapMetricVal'
dataFrameToPlot$areaUnderCurve <- as.numeric(as.character(dataFrameToPlot$areaUnderCurve))
ggplot(dataFrameToPlot, aes(x=as.factor(qapMetricVal),y=areaUnderCurve, fill=areaUnderCurve)) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="Area Under ROC Curve Predicting QC 0 vs QC 2", x="QAP Measures", y="Area Under ROC Curve") + coord_cartesian(ylim=c(.45,1))
dev.off()

###################
## Create all of ##
## the glm models##
## For all 0 v!0 ##
##     data      ##
###################
response <- as.numeric(unlist(mergedQAP[manualQAValue]))
outcome.done <- mergedQAP$bblid.y
flip.index <- c(0,0,1,1,1,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,1,0,1,1)
flip.index <- c(0,0,1,1,1,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
count <- 1
pdf('roc_curve_bad_vs_all_else.pdf')
for(qapVal in qapValNames){
  outcome.response <- rep(0, nrow(mergedQAP))
  outcome <- as.numeric(unlist(mergedQAP[qapVal]))
  roc.tmp <- roc(response ~ outcome)
  plot(roc.tmp, main=qapVal, print.thres=TRUE, print.thrs.best.method="closest.topleft")
  legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
  output.coords.tmp <- coords(roc.tmp, 'best', ret=c('tn', 'tp', 'fp', 'fn','npv', 'ppv', 'threshold',"specificity", "sensitivity"))
  output.coords.tmp <- append(output.coords.tmp, auc(roc.tmp))
  tmp.thresh <- coords(roc.tmp,'best')[1][[1]]
  if(flip.index[count] == 1){
    bblid.index <- which(outcome < tmp.thresh)
  }
  if(flip.index[count] == 0){
    bblid.index <- which(outcome > tmp.thresh)
  }
  if(count == 1){
    output.coords <- output.coords.tmp
  }
  if(count > 1){
    output.coords <- cbind(output.coords, output.coords.tmp)
  }
  print(length(bblid.index))
  outcome.response[bblid.index] <- 1
  outcome.done <- cbind.all(outcome.done, outcome.response)
  count <- count + 1 
} 
sumAllOutcomes <- rowSums(outcome.done[,-1])
roc.tmp <- roc(response ~ sumAllOutcomes)
plot(roc.tmp, main= 'Total Flag QAP Vals', print.thres=TRUE, print.thrs.best.method="closest.topleft")
legend(x='bottomright', legend=paste('AUC = ', auc(roc.tmp)))
output.coords.tmp <- coords(roc.tmp, 'best', ret=c('tn', 'tp', 'fp', 'fn','npv', 'ppv', 'threshold',"specificity", "sensitivity"))
output.coords.tmp <- append(output.coords.tmp, auc(roc.tmp))
output.coords <- cbind(output.coords, output.coords.tmp)
dev.off()

#######################
## Now plot the AUC  ##
## For all ROC Curves##
## For 0 vs !0 data  ##
#######################
pdf('predictingGoodVsAllElse.pdf')
dataFrameToPlot <- as.data.frame(cbind(qapValNames, output.coords[10,1:36]))
colnames(dataFrameToPlot)[2] <- 'areaUnderCurve'
colnames(dataFrameToPlot)[1] <- 'qapMetricVal'
dataFrameToPlot$areaUnderCurve <- as.numeric(as.character(dataFrameToPlot$areaUnderCurve))
ggplot(dataFrameToPlot, aes(x=as.factor(qapMetricVal),y=areaUnderCurve, fill=areaUnderCurve)) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="Area Under ROC Curve Predicting QC 0 vs QC !0", x="QAP Measures", y="Area Under ROC Curve") + coord_cartesian(ylim=c(.45,.9))
dev.off()


#######################
##Now work with Multi##
## Multinomial ROC   ##
## For all QAP Values##
#######################


#
#
#
#
#
#
#
## This is experimental *****
pdf('tmp.pdf')
for(qapVal in qapValNames){
  print(multiclass.roc(predictor=as.numeric(unlist(mergedQAP[qapVal])), response=as.numeric(unlist(mergedQAP[manualQAValue])),
                 formula=response~predictor))
}
dev.off()

#
#
#
#
#
#
#
#















##*********************************##
## To do for this previous for loop##
## Figure out the cutoffs and whether 
## it is beter to be higher and lower
## after this is distinguished find the bblids
## That exceed or are below the threshold
## and relate this to the average rating.
## I will then need to fine tune 
## In the mean time though I can 
## Just graph the output.coods and share that 
## with the group... it looks like I just need to explore
## Creating the roc curves with a training data set
## Hopefully this will increase the sensativity
##*********************************##



##*********************************##
## Next thing I need to do is run each 
## qap value and run it through the lm 
## command with a formula that looks like
## lm(avgDataQuality ~ qap Measure)
## Might add some covariates in here later
##*********************************##

## First grab all of the p values
lMOutcome <- as.numeric(unlist(mergedQAP['rawAverageRating']))
pValDataFrame <- data.frame()
for(qapVal in qapValNames){
  predictor <- as.numeric(unlist(mergedQAP[qapVal]))
  outputLM <- lm( lMOutcome ~ predictor, data = mergedQAP)
  pVal <- summary(outputLM)$coefficients[2,4]
  pValDataFrame <- rbind(pValDataFrame, cbind(qapVal, pVal))
}

## Now lets grab all of the r values between rating bin and qap measure
rValDataFrame <- data.frame()
for(qapVal in qapValNames){
  predictor <- as.numeric(unlist(mergedQAP[qapVal]))
  outputR <- cor(lMOutcome, predictor)
  rValDataFrame <- rbind(rValDataFrame, cbind(qapVal, outputR))
}

## Now plot the r values
pdf('corBtnQAPValsAndAllImages.pdf', width=12)
ggplot(rValDataFrame, aes(x=as.factor(qapVal),y=as.numeric(as.character(outputR)), fill=as.numeric(as.character(outputR)))) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="Cor btn Average Rating and QAP Values for All Images", x="QAP Measures", y="Corellation") + coord_cartesian(ylim=c(-.3,.3))
dev.off()

## First grab all of the p values for all !0 average rating images
lMOutcome <- as.numeric(unlist(mergedQAP[manualQAValue]))[which(mergedQAP$averageRating!=0)]
pValDataFrame <- data.frame()
for(qapVal in qapValNames){
  predictor <- as.numeric(unlist(mergedQAP[qapVal]))[which(mergedQAP$averageRating!=0)]
  outputLM <- lm( lMOutcome ~ predictor, data = mergedQAP)
  pVal <- summary(outputLM)$coefficients[2,4]
  pValDataFrame <- rbind(pValDataFrame, cbind(qapVal, pVal))
}

## Now lets grab all of the r values between rating bin and qap measure for !0 images
rValDataFrame <- data.frame()
for(qapVal in qapValNames){
  predictor <- as.numeric(unlist(mergedQAP[qapVal]))[which(mergedQAP$averageRating!=0)]
  outputR <- cor(lMOutcome, predictor)
  rValDataFrame <- rbind(rValDataFrame, cbind(qapVal, outputR))
}

## Now plot the r values
pdf('corBtnQAPValsAndAllNotBadImages.pdf', width=12)
ggplot(rValDataFrame, aes(x=as.factor(qapVal),y=as.numeric(as.character(outputR)), fill=as.numeric(as.character(outputR)))) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="Cor btn Average Rating and QAP Values for All !0 Images", x="QAP Measures", y="Corellation") + coord_cartesian(ylim=c(-.3,.3))
dev.off()

## Now lets do partial correllation 
p.cor.data.frame <- data.frame()
confound.string <- c("ageAtGo1Scan", "sex", "race2")
for(qapVal in qapValNames){
  allGrepPatternCols <- grep("mprage_fs_ct", names(mergedQAP))
  allGrepPatternCols <- append(2321, allGrepPatternCols)
  p.cor.vector <- vector()
  for(grepPatternCol in allGrepPatternCols){
    p.cor.string <- c(names(mergedQAP)[grepPatternCol], qapVal, confound.string) 
    tmp <- mergedQAP[complete.cases(mergedQAP[,grepPatternCol]),]
    #tmp <- tmp[complete.cases(tmp$mprageMassICV),]
    p.cor.val <- pcor(p.cor.string, var(tmp[p.cor.string]))
    p.cor.vector <- append(p.cor.vector, p.cor.val)  
  }
  p.cor.data.frame <- rbind(p.cor.data.frame, p.cor.vector)
  print(paste("done with ", qapVal, sep =''))
}
colnames(p.cor.data.frame) <- names(mergedQAP)[allGrepPatternCols]
rownames(p.cor.data.frame) <- qapValNames

rValDataFrame <-as.data.frame(cbind(qapValNames, p.cor.data.frame$mprage_fs_mean_thickness))
colnames(rValDataFrame) <- c('qapVal', 'outputR')
pdf('pCorMeanThickness.pdf', width=12)
ggplot(rValDataFrame, aes(x=as.factor(qapVal),y=as.numeric(as.character(outputR)), fill=as.numeric(as.character(outputR)))) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="PCor btn FS Mean Thickness and QAP Values for All Images", x="QAP Measures", y="Corellation") + coord_cartesian(ylim=c(-.25,.25))
dev.off()

## Now do partial correllation while excluding 0 rated images
p.cor.data.frame <- data.frame()
confound.string <- c("ageAtGo1Scan", "sex", "race2")
for(qapVal in qapValNames){
  allGrepPatternCols <- grep("mprage_fs_ct", names(mergedQAP))
  allGrepPatternCols <- append(2321, allGrepPatternCols)
  p.cor.vector <- vector()
  for(grepPatternCol in allGrepPatternCols){
    p.cor.string <- c(names(mergedQAP)[grepPatternCol], qapVal, confound.string) 
    tmp <- mergedQAP[which(mergedQAP[manualQAValue]!=0),]
    tmp <- mergedQAP[complete.cases(mergedQAP[,grepPatternCol]),]
    #tmp <- tmp[complete.cases(tmp$mprageMassICV),]
    p.cor.val <- pcor(p.cor.string, var(tmp[p.cor.string]))
    p.cor.vector <- append(p.cor.vector, p.cor.val)  
  }
  p.cor.data.frame <- rbind(p.cor.data.frame, p.cor.vector)
  print(paste("done with ", qapVal, sep =''))
}
colnames(p.cor.data.frame) <- names(mergedQAP)[allGrepPatternCols]
rownames(p.cor.data.frame) <- qapValNames

rValDataFrame <-as.data.frame(cbind(qapValNames, p.cor.data.frame$mprage_fs_mean_thickness))
colnames(rValDataFrame) <- c('qapVal', 'outputR')
pdf('pCorMeanThicknessWO0Images.pdf', width=12)
ggplot(rValDataFrame, aes(x=as.factor(qapVal),y=as.numeric(as.character(outputR)), fill=as.numeric(as.character(outputR)))) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="PCor btn FS Mean Thickness and QAP Values For All !0 Images", x="QAP Measures", y="Corellation") + coord_cartesian(ylim=c(-.25,.25))
dev.off()

### Now plot some relationships between cortical thickness and qap values
pdf('test4.pdf')
mergedQAP$averageRating <- as.factor(mergedQAP$averageRating)
fit <- lm(mprage_fs_mean_thickness ~ bg.kurtosis+averageRating, data = mergedQAP)
visreg(fit, "bg.kurtosis", by="averageRating", overlay=TRUE)
dev.off()

## Now do more partial corellation this time between ct measures and age
## When regressing out sex and race w & w/o qap measures
## Now do partial correllation while excluding 0 rated images
p.cor.data.frame <- data.frame()
for(qapVal in c('bg.kurtosis', 'qi1', 'bg.skewness')){
  confound.string <- c("sex", "race2")
  allGrepPatternCols <- grep("mprage_fs_ct", names(mergedQAP))
  allGrepPatternCols <- append(2321, allGrepPatternCols)
  confound.string <- append(confound.string, qapVal)
  p.cor.vector <- vector()
  for(grepPatternCol in allGrepPatternCols){
    p.cor.string <- c(names(mergedQAP)[grepPatternCol], 'ageAtGo1Scan', confound.string) 
    tmp <- mergedQAP[which(mergedQAP[manualQAValue]!=0),]
    tmp <- mergedQAP[complete.cases(mergedQAP[,grepPatternCol]),]
    #tmp <- tmp[complete.cases(tmp$mprageMassICV),]
    p.cor.val <- pcor(p.cor.string, var(tmp[p.cor.string]))
    p.cor.vector <- append(p.cor.vector, p.cor.val)  
  }
  p.cor.data.frame <- rbind(p.cor.data.frame, p.cor.vector)
  print(paste("done with ", qapVal, sep =''))
}
confound.string <- c("sex", "race2")
allGrepPatternCols <- grep("mprage_fs_ct", names(mergedQAP))
allGrepPatternCols <- append(2321, allGrepPatternCols)
p.cor.vector <- vector()
for(grepPatternCol in allGrepPatternCols){
  p.cor.string <- c(names(mergedQAP)[grepPatternCol], 'ageAtGo1Scan', confound.string) 
  tmp <- mergedQAP[which(mergedQAP[manualQAValue]!=0),]
  tmp <- mergedQAP[complete.cases(mergedQAP[,grepPatternCol]),]
  #tmp <- tmp[complete.cases(tmp$mprageMassICV),]
  p.cor.val <- pcor(p.cor.string, var(tmp[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)  
}
p.cor.data.frame <- rbind(p.cor.data.frame, p.cor.vector)
print(paste("done with ", 'none', sep =''))

colnames(p.cor.data.frame) <- names(mergedQAP)[allGrepPatternCols]
rownames(p.cor.data.frame) <- c('bg.kurtosis', 'qi1', 'bg.skewness', 'None')
rValDataFrame <-as.data.frame(cbind(c('bg.kurtosis', 'qi1', 'bg.skewness', 'None'), p.cor.data.frame$mprage_fs_mean_thickness))
colnames(rValDataFrame) <- c('qapVal', 'outputR')
pdf('pCorAgevsCTWithQAPandWithoutQAP.pdf', width=12)
ggplot(rValDataFrame, aes(x=as.factor(qapVal),y=as.numeric(as.character(outputR)), fill=as.numeric(as.character(outputR)))) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="PCor btn QAP Regressed vs Non Regressed FSCT", x="QAP Measures", y="Corellation") #+ coord_cartesian(ylim=c(-.25,.25))
dev.off()



## Now play around with differences with cortical thickness when regressing for qap Values vs when not
pdf('qapValRegressing.pdf')
fit <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + sex + race2 + bg.kurtosis, data=mergedQAP) 
visreg(fit, "ageAtGo1Scan", by="sex", overlay=TRUE, ylim=c(2.2,3), main="FSCT predicted by age w/ bg.kurtosis regresion",legend="FALSE")
fit <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + sex + race2, data=mergedQAP)
visreg(fit, "ageAtGo1Scan", by="sex", overlay=TRUE, ylim=c(2.2,3), main="FSCT predicted by age w/o QAP regression", legend="FALSE")
dev.off()

# Now look at individual CT regions and find differences in significanlty predicitng the CT
# with age when regressing with and without QAP measures



################
################
##Play around ##
## Step AIC   ##
## Build a  Formula with the output of stepAIC ##
## From that use thatf romula for several predicition methods ##
## Such as naive bayes, svm, roc - and what ever else I can throw at the computer ##
################
################ 

## First thing is to build a formula
stepData <- goodvsBadData
stepData$averageRating <- as.numeric(as.character(stepData$averageRating))
step.model.prep <- lm(averageRating ~ ., stepData)
step.lm.formula <- stepAIC(step.model.prep, k=2)$call
step.lm.formula <- step.lm.formula$formula

## Now that we have the formula from step AIC lets do some predicition starting with ROC
multi.roc.curve <- multinom(formula = step.lm.formula, data = mergedQAP)
predict(multi.roc.curve, newdata=goodvsBadData, type='prob')

## Now do 0 vs !0
multinom.data.with.train <- mergedQAP
multinom.data.with.train$averageRating <- rep(1, nrow(mergedQAP))
multinom.data.with.train$averageRating[which(mergedQAP$averageRating==0)] <- 0
multinom.bad.index <- which(mergedQAP$averageRating==0)
multinom.good.index <- which(mergedQAP$averageRating!=0)
output.mislabeled.vec <- vector()
output.mislabeled.zeros <- vector()
output.mislabeled.ones <- vector()
for(iVal in seq(1,1000,1)){
  rand.train.sample.index <- append(sample(multinom.bad.index, 40, replace = F), sample(multinom.good.index, 980, replace=F))
  rand.train.sample <- multinom.data.with.train[rand.train.sample.index,]
  multi.roc.curve <- multinom(formula = step.lm.formula, data = rand.train.sample)
  outcome.predicition <- predict(multi.roc.curve, newdata= multinom.data.with.train[-rand.train.sample.index,])
  output.mislabeled.vec <- append(output.mislabeled.vec,
                                 length(which(outcome.predicition != multinom.data.with.train$averageRating[-rand.train.sample.index])))
  true.zero.index <- which(multinom.data.with.train$averageRating[-rand.train.sample.index]==0)
  true.one.index <- which(multinom.data.with.train$averageRating[-rand.train.sample.index]==1)
  length.zero <- length(which(multinom.data.with.train$averageRating[true.zero.index]!=outcome.predicition[true.zero.index]))
  length.one <- length(which(multinom.data.with.train$averageRating[true.one.index]!=outcome.predicition[true.one.index]))
  output.mislabeled.zeros <- append(output.mislabeled.zeros, length.zero)
  output.mislabeled.ones <- append(output.mislabeled.ones, length.one)
}

