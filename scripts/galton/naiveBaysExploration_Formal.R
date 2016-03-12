# AFGR October 22 2015

# This script is going to be used to perform several tasks within the T1QA project which are listed below:
#	1.) Explain population demographics
#	2.) Explain QAP measures
#	3.) Explore relationship between demographics and QAP measures
#	4.) Explore relationship between QAP measures and manual SBIA QA


## Load Library(s)
library(corrplot)
library(ggplot2)
library(psych)
library(e1071)
library(pROC)

## Declare some variables
manualQAValue <- C("")

## Declare Functions to use
# summarySE used to gather explanatory statistics from subject groups
# Taken from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
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

# Declare a function which will return the LM of regressed QAP values predicting cortical thickness
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
   regressedValues <- lm(dataFrame[,colToResid]~dataFrame[,grepQA]+ageCol+ageSquared+ageCubed
                                               +sexCol+raceCol+icvCol, data=dataFrame, 
                                               na.action=na.omit)
   return(regressedValues)
} 

## Load the data 
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

# Now merge the data 
mergedQAP <- merge(qapRawOutput, manualQAData, by="scanid")


## Now begin describing the data 
# Create a pdf to output all subject explanatory information 
pdf(file="subjectExplanatoryInformation.pdf")
## Begin with subject age's
# Whole population
hist(mergedQAP$ageAtGo1Scan/12, main="Distribution of QAP Subject Age", xlab="Subject Age in Years")
# Flagged Images
hist(mergedQAP$ageAtGo1Scan[which(mergedQAP$mprageSbiaExclude==1)]/12, main="Distribution of Flagged Subject Age", xlab="Subject Age in Years")
# Non-Flagged Images
hist(mergedQAP$ageAtGo1Scan[which(mergedQAP$mprageSbiaExclude==0)]/12, main="Distribution of Non-Flagged Subject Age", xlab="Subject Age in Years")
# Now perform a t test on the two groups 


# Now Whole Population Gender
barplot(table(mergedQAP$sex), main = "n of Gender", xlab = "Gender", ylab = "n of Gender", names.arg=c("male", "female"))

# Now Flagged Images Gender
barplot(table(mergedQAP$sex[which(mergedQAP$mprageSbiaExclude==1)]), main = "n of Gender for Flagged Images", xlab = "Gender", ylab = "n of Gender", names.arg=c("male", "female"))

# Now Non-Flagged Images Gender
barplot(table(mergedQAP$sex[which(mergedQAP$mprageSbiaExclude==0)]), main = "n of Gender for Non-Flagged Images", xlab = "Gender", ylab = "n of Gender", names.arg=c("male", "female"))




