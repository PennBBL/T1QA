# AFGR September 24 2015 


# This R script is just going to be used to produce some basic plots of the qap output for the PNC imaging data 
# I will be producing histograms of the values, heat maps between values and bar charts with error bars for pass vs fail measures 

# Load libray(s)
library(corrplot)
library(ggplot2)
library(psych)
library(e1071)
library(pROC)


# Function declaration
# SummarySe function taken from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

# Create a function which will reutnr the residualized values for the row of interest
residualizeInputRow <- function(dataFrame, colToResid, qaMetricVal){
   qaCol <- colsToHist[qaMetricVal]
   grepQA <- grep(qaCol, names(dataFrame))
   if(length(grepQA) > 1 ){
     grepQA <- grepQA[1]
   }
   raceCol <- dataFrame$race2
   sexCol <- dataFrame$sex
   ageCol <- dataFrame$ageAtGo1Scan
   icvCol <- dataFrame$mprageMassICV
   ageSquared <- ageCol^2
   ageCubed <- ageCol^3
   regressedValues <- lm(dataFrame[,colToResid]~dataFrame[,grepQA]+ageCol+ageSquared+ageCubed+sexCol+raceCol+icvCol, data=dataFrame, na.action=na.omit)
   return(regressedValues)
} 

# Begin by loading the data
qapRawOutput <- read.csv("/home/adrose/qapQA/data/qap_anatomical_spatial_.csv")
kurtVals <- read.csv("/home/adrose/qapQA/data/kurtAndSkewVals.txt", sep=' ')
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
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

# Now lets see if we cant merge this data
mergedQAP <- merge(qapRawOutput, manualQAData, by="scanid")
mergedQAP <- mergedQAP[!duplicated(mergedQAP),]

# Create the histograms for the qap Output in a pdf file
colsToHist <- names(qapRawOutput)[4:29]
pdf(file="histsOfQAPData.pdf")
for(valueToHist in colsToHist){
  mainTitle <- paste("Distribution of", valueToHist, sep=" ")
  xAxisTitle <- paste(valueToHist, "Values", sep=" ")
  valueToHistCol <- grep(valueToHist, names(mergedQAP))
  if(length(valueToHistCol) > 1){
    valueToHistCol <- valueToHistCol[1]
    }
  hist(mergedQAP[,valueToHistCol], main = mainTitle, xlab=xAxisTitle)
}
dev.off()

# Now create a correllation plot of all of the QAP values of interest
pdf(file="corrMatrixQAPData.pdf")
corrplot(cor(mergedQAP[,colsToHist]), method="circle")
corrplot(cor(mergedQAP[,colsToHist]), method="ellipse")
dev.off()

# Now create a bar graph with standard error for each QAP value based on the manual QA
# This is going to be done in a for loop 
pdf(file="barGraphsFromQAPDataWithSBIAExclude.pdf")
for(valueToBarPlot in colsToHist){
  pdf(file=paste(valueToBarPlot,".pdf",sep=''))
  mainTitle <- paste("Average", valueToBarPlot, "Value vs Manual QA", sep=" ")
  yAxisTitle <- paste("Average", valueToBarPlot, "Value", sep=" ")
  csvFileName <- paste(valueToBarPlot,"_Stats.csv", sep="")
  foo <- summarySE(mergedQAP, measurevar=valueToBarPlot, groupvars="mprageSbiaExclude")
  write.csv(foo, file=csvFileName)
  barPlotToPrint <- ggplot(foo, aes(x=factor(mprageSbiaExclude), y=foo[,3], fill=factor(mprageSbiaExclude))) + 
                           geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                           geom_errorbar(aes(ymin=foo[,3]-se, ymax=foo[,3]+se), width = .2, position=position_dodge(.9)) + 
                           ggtitle(mainTitle) + 
                           xlab("Manual QA Value") +
                           ylab(yAxisTitle)
  print(barPlotToPrint)    
  dev.off()
}
dev.off()

## Now perform a t-test to compare the two means for those that passed and those that did not pass the qa metric and those that did 
# Declare the QA binary identifiers that we are going to investigate against
qaMetricList <- c("mprageFsExclude", "mprageFsQaExclude", "mprageFSautoqaExclude", #"mprageSbiaRavensQaExclude", 
                  #"mprageSbiaMarsQaExclude", 
                  "mprageAntsExclude")
# Declare an empty frame with the proper column names 
dataFrameToExport <- data.frame(QAP_Factor <-character(0), QA_Method <- character(0), N_of_Pass <- numeric(0), Mean_of_Pass <- numeric(0), N_of_Fail <- numeric(0), Mean_of_Fail <- numeric(0), P_Value <- numeric(0))
# Run through a for loop for each QA metric 
for(qaMetric in qaMetricList){
  # Run through a for loop for each QAP value
  for(valueToTest in colsToHist){
    valueToTestCol <- grep(valueToTest, names(mergedQAP))
    if(length(valueToTestCol) > 1){
      valueToTestCol <- valueToTestCol[1]
      }
    qaMetricCol <- grep(qaMetric, names(mergedQAP))
    passImages <- which(mergedQAP[,qaMetricCol] == 0)
    passImages <- mergedQAP[passImages,]
    failImages <- which(mergedQAP[,qaMetricCol] == 1)
    failImages <- mergedQAP[failImages,]
    foo <- summarySE(mergedQAP, measurevar=valueToTest, groupvars=qaMetric)
    bar <- t.test(as.numeric(passImages[,valueToTestCol]), as.numeric(failImages[,valueToTestCol]))
    for(rowValue in 2:3){
      rowToBind <- valueToTest
      rowToBind <- cbind(rowToBind, qaMetric)
      rowToBind <- cbind(rowToBind, foo[1,2])
      rowToBind <- cbind(rowToBind, foo[1,3])
      rowToBind <- cbind(rowToBind, foo[2,2])
      rowToBind <- cbind(rowToBind, foo[2,3])
      rowToBind <- cbind(rowToBind, bar$p.value)
      #rowToBind <- as.numeric(rowToBind)
    }
    dataFrameToExport <- rbind(dataFrameToExport, rowToBind) 
  }
}


### Now explore the relationship between those significant QAP measures and cortical thickness
# Start by declaring the variables of interest
#allCTMeasureCol <- grep("mprage_fs_ct", names(mergedQAP))
allMarsVolMeasures <- grep("mprage_mars_vol", names(mergedQAP))
#allFSVolMeasures <- grep("mprage_fs_vol", names(mergedQAP))


# Now declare the output variables
dataFrameToExport <- data.frame()

# Loop thorugh each QAP measure 
for(qapMetricVal in 1:length(colsToHist)){
 print(qapMetricVal)
 qapMetric <- colsToHist[qapMetricVal]
 vectorToBind <- vector()
 # Loop through each cortical thickness measurment
 for(ctCol in allMarsVolMeasures){
   fittedPVal <- summary(residualizeInputRow(mergedQAP, ctCol, qapMetricVal))$coefficients[2,4]   
   vectorToBind <- append(vectorToBind, fittedPVal)
 }
 dataFrameToExport <- rbind(dataFrameToExport, vectorToBind)
}
colnames(dataFrameToExport) <- names(mergedQAP)[allMarsVolMeasures]
rownames(dataFrameToExport) <- colsToHist

# I now have a data frame with significance values for the QAP measures vs the cortical thickness values, regressed for age sex and race.
# I need to plot the n of significant values vs qap measures, just a bar plot will suffice
# Start by binariing significant values from the p value dataframe
barPlotDataFrame <- ifelse(dataFrameToExport[,1] <0.05,1,0)
for(colIndex in 2:ncol(dataFrameToExport)){
  colToBind <- ifelse(dataFrameToExport[,colIndex]<0.05,1,0)
  barPlotDataFrame <- cbind(barPlotDataFrame, colToBind)
}
barPlotDataFrame <- as.data.frame(barPlotDataFrame)
rownames(barPlotDataFrame) <- colsToHist
colnames(barPlotDataFrame) <- names(mergedQAP)[allMarsVolMeasures]

# Now plot the bar graph
barGraphData <- as.data.frame(rowSums(barPlotDataFrame))
barGraphData$qapMeasures <- colsToHist
dataToPlot <- barGraphData[,2]
dataToPlot <- cbind(dataToPlot,barGraphData[,1])
dataToPlot <- as.data.frame(dataToPlot)
dataToPlot$V2 <- as.numeric(as.character(dataToPlot$V2))
minVal <- min(as.numeric(as.character(dataToPlot$V2)))-4
maxVal <- max(as.numeric(as.character(dataToPlot$V2)))+2
pdf(file="barPlotOfSigCTValsVsQAPMeasures.pdf")
ggplot(dataToPlot, aes(x=dataToPlot,y=V2, fill=V2)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="# Sig Vol Vals vs QAP Measures", x="QAP Measures", y="# of Sig  Values") + scale_y_continuous(breaks=seq(minVal,maxVal,3))
dev.off()



# I am now going to run through each variable in cols to hist and put it through the naive bayes predicition basics to #find out how well it can differentiate between good and bad images by it self.

# I will then make a bar graph of # predicted to be bad images for each modality
# Create a df with just our varibales of interest 
isolatedVars <- mergedQAP[4:30]
foo <- mergedQAP$mprageSbiaExclude
isolatedVars <- cbind(isolatedVars,foo)
isolatedVars$foo <- as.factor(isolatedVars$foo)
bar <- mergedQAP$ageAtGo1Scan
isolatedVars <- cbind(isolatedVars,bar)
isolatedVars$bar <- as.factor(isolatedVars$bar)


dataFrameToPlot <- data.frame()

for(qapVal in colsToHist){
  qapCol <- grep(qapVal, names(isolatedVars))
    if(length(qapCol) > 1){
      qapCol <- qapCol[1]
    }
  model <- naiveBayes(foo~isolatedVars[,qapCol], data=isolatedVars)
  predicVals <- table(predict(model, isolatedVars))
  flagsPredic <- unname(predicVals[2])
  vectorToAppend <- cbind(qapVal, flagsPredic)
  dataFrameToPlot <- rbind(dataFrameToPlot, vectorToAppend)
}

## Work with logistic regression down here!!!
dataFrameToPlot <- data.frame()

for(qapVal in colsToHist){
  qapCol <- grep(qapVal, names(isolatedVars))
    if(length(qapCol) > 1){
      qapCol <- qapCol[1]
    }
  model <- glm(foo~isolatedVars[,qapCol], data=isolatedVars, family="binomial")
  predicVals <- predict(model, newdata=isolatedVars, type='response')
  areaUnderCurve <- auc(roc(response=foo, predictor=predicVals, levels=c(0,1), auc=TRUE, ci=TRUE, plot.roc=TRUE))
  vectorToAppend <- cbind(qapVal, areaUnderCurve)
  dataFrameToPlot <- rbind(dataFrameToPlot, vectorToAppend)
}
dataFrameToPlot$areaUnderCurve <- as.numeric(as.character(dataFrameToPlot$areaUnderCurve))
pdf(file="areaUnderROCCurveForAllQAPVals.pdf")
ggplot(dataFrameToPlot, aes(x=as.factor(qapVal),y=areaUnderCurve, fill=areaUnderCurve)) + geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) + theme(axis.text.x = element_text(angle=45,hjust=1)) + labs(title="Area Under ROC Curve Predicting mprageSbiaExclude", x="QAP Measures", y="Area Under ROC Curve") + coord_cartesian(ylim=c(.4,.75)) + theme(legend.position='none')
dev.off()




