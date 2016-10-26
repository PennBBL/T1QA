# AFGR October 24 2016
# This script is going to be used to compute the effect size of the outcome of the 1 vs 2 model on individual ROI values
# cortical thickness and volume values will be being predicted as well as GMD just for the heck of it.
# This will be done in both hemispheres which is also important to note.
# The various steps involved in this process are:
#	0.) Produce age regressed values : may or may not include this
#	1.) Model our individual ROI following this method: lm(individualROI ~ qapValues)
#	2.) Perform FDR correction on all ROI's 


## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

## Declare any functions neccassary 
# Create a function which will regress out age
regressAge <- function(dataFrame, valsToRegress){
  # First thing lets make sure we return values of the same length
  outputValues <- rep(NA, nrow(dataFrame))

  # Now we need to get an index of values to replace
  index <- which(complete.cases(valsToRegress)=='TRUE')

  # Now lets create our new values
  ageVals <- dataFrame$ageAtGo1Scan
  outputVals <- lm( valsToRegress ~ ageVals)$residuals

  # Now lets make our output values
  outputValues[index] <- outputVals
  return(outputValues)
}

# Now create a function which will regress out sex
regressSex <- function(dataFrame, valsToRegress){
  # First thing lets make sure we return values of the same length
  outputValues <- rep(NA, nrow(dataFrame))

  # Now we need to get an index of values to replace
  index <- which(complete.cases(valsToRegress)=='TRUE')

  # Now lets create our new values
  sexVals <- dataFrame$sex
  outputVals <- lm( valsToRegress ~ sexVals)$residuals

  # Now lets make our output values
  outputValues[index] <- outputVals
  return(outputValues)
}

# Now cretae a function which will reutnr the p val for the prediction of the imaging value 
# based on the quality index with age and sex regressed values
returnPVal <- function(imagingVal, qualityVal, df, regressAgeBOO=TRUE, regressSexBOO=TRUE){
  if(regressAgeBOO == 'TRUE'){
    imagingVal <- regressAge(df, imagingVal)
    qualityVal <- regressAge(df, qualityVal)
  }
  if(regressSexBOO == 'TRUE'){
    imagingVal <- regressSex(df, imagingVal)
    qualityVal <- regressSex(df, qualityVal)
  }
  outputPVal <- cor.test(imagingVal, qualityVal, method='kendall')$p.value
    
  return(outputPVal)
}

# Now create a function which will return all of the pVals for a specific grep pattern
# which will be the prefix for an imaging value
pvalLoop <- function(grepPattern, dataFrame){
  # First lets find the values that we need to loop through
  colVals <- grep(grepPattern, names(dataFrame))

  # Now lets compute our p vals 
  outputPVals <- apply(dataFrame[,colVals], 2, function(x) returnPVal(x ,dataFrame$oneVsTwoOutcome, all.train.data))

  # Now fdr correct these suckers
  outputPVals.fdr <- p.adjust(outputPVals, method='fdr')
  output <- cbind(names(outputPVals.fdr), as.numeric(unname(outputPVals.fdr)))
  return(output)
}
## Load library(s) we will need
install_load('caret', 'lme4')

## Now lets prep the data
## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now produce all of our p vals
fsCTVals <- pvalLoop('mprage_fs_ct', all.train.data)
jlfCTVals <- pvalLoop('mprage_jlf_ct', all.train.data)
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
fsVolVals <- pvalLoop('mprage_fs_vol', all.train.data)

## Now I need to export these in a manner that will make it easy to create a brain picture =D 
write.csv(fsCTVals, 'fsSigQAPROIct.csv', quote=F)
write.csv(jlfCTVals, 'jlfSigQAPROIct.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmd.csv', quote=F)
write.csv(fsVolVals, 'fsSigQAPROIvol.csv', quote=F)

