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
tbvData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_antsCtVol.csv')

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

# Now create a function which will regress out TBV
regressSex <- function(dataFrame, valsToRegress){
  # First thing lets make sure we return values of the same length
  outputValues <- rep(NA, nrow(dataFrame))

  # Now we need to get an index of values to replace
  index <- which(complete.cases(valsToRegress)=='TRUE')

  # Now lets create our new values
  tbvVals <- dataFrame$sex
  outputVals <- lm( valsToRegress ~ sexVals)$residuals

  # Now lets make our output values
  outputValues[index] <- outputVals
  return(outputValues)
}

# Now cretae a function which will reutnr the p val for the prediction of the imaging value 
# based on the quality index with age and sex regressed values
returnPVal <- function(imagingVal, qualityVal, regVals, df, regressAgeBOO=TRUE, regressSexBOO=TRUE, regressTBV=FALSE){
  if(regressAgeBOO == 'TRUE'){
    imagingVal <- regressAge(df, imagingVal)
    qualityVal <- regressAge(df, qualityVal)
  }
  if(regressSexBOO == 'TRUE'){
    imagingVal <- regressSex(df, imagingVal)
    qualityVal <- regressSex(df, qualityVal)
  }
  if(regressTBV == 'TRUE'){
    regVals <- paste(regVals, 'df$mprage_antsCT_vol_TBV', sep='+')
  }
  form <- as.formula(paste('imagingVal~qualityVal', paste(regVals), sep=''))
  outputPVal <- summary(lm(form))$coefficients[2,4]
  #outputPVal <- cor.test(imagingVal, qualityVal, method='kendall')$p.value
 
  return(outputPVal)
}

returnTVal <- function(imagingVal, qualityVal, regVals, df, regressAgeBOO=TRUE, regressSexBOO=TRUE, regressTBV=FALSE){
  if(regressAgeBOO == 'TRUE'){
    imagingVal <- regressAge(df, imagingVal)
    qualityVal <- regressAge(df, qualityVal)
  }
  if(regressSexBOO == 'TRUE'){
    imagingVal <- regressSex(df, imagingVal)
    qualityVal <- regressSex(df, qualityVal)
  }
  if(regressTBV == 'TRUE'){
    regVals <- paste(regVals, 'df$mprage_antsCT_vol_TBV', sep='+')
  }
  form <- as.formula(paste('imagingVal~qualityVal', paste(regVals), sep=''))
  outputPVal <- summary(lm(form))$coefficients[2,3]
  #outputPVal <- cor.test(imagingVal, qualityVal, method='kendall')$p.value
 
  return(outputPVal)
}



# Now create some functions which will be used to create the output colored labels for itksnap
# this is all super experimental and I am not even sure if it will work 
# but lets start by trying =/
returnHeatMapITKSnapVals <- function(inputZScores, lowColor='blue', hiColor='red'){
  # Create some functions this function will call... yeesh
  range01 <- function(x)(x-min(x))/diff(range(x))
  colfunc <- colorRampPalette(c("blue", "red"))
  cRamp <- function(x){
    cols <- colorRamp(c(lowColor, hiColor))(range01(as.numeric(x)))
  }   
  # Output values
  outputValues <- matrix(0, nrow=(length(inputZScores)+1), ncol=8)

  # Now cretae our rgb values
  redValues <- round(cRamp(inputZScores)[,1], digits=0)
  greenValues <- round(cRamp(inputZScores)[,2], digits=0)
  blueValues <- round(cRamp(inputZScores)[,3], digits=0)

  # First lets create our index column
  outputValues[,1] <- seq(0, length(inputZScores))

  # Now put the proper values in the correct place 
  outputValues[2:(length(inputZScores)+1),2] <- redValues
  outputValues[2:(length(inputZScores)+1),3] <- greenValues
  outputValues[2:(length(inputZScores)+1),4] <- blueValues

  # Now we need to do the Transperancy column
  outputValues[,5] <- c(0, rep(1, length(inputZScores)))

  # Now the visibility column
  outputValues[,6] <- c(0, rep(1, length(inputZScores)))

  # Now the mesh visibility
  outputValues[,7] <- c(0, rep(1, length(inputZScores)))

  # Now the label indicies 
  labelIndexNames <- c('Clear Label', paste('Label ', inputZScores, sep=''))
  labelIndexNames <- paste('"', labelIndexNames, '"', sep='')
  outputValues[,8] <- labelIndexNames
  
  # Now return our output
  return(outputValues)
}


# Now create a function which will return all of the pVals for a specific grep pattern
# which will be the prefix for an imaging value
pvalLoop <- function(grepPattern, dataFrame, TBV=FALSE){
  # First lets find the values that we need to loop through
  colVals <- grep(grepPattern, names(dataFrame))

  # Now lets compute our p vals 
  outputPVals <- apply(dataFrame[,colVals], 2, function(x) returnPVal(x ,dataFrame$averageRating, '+df$ageAtGo1Scan+df$sex+df$ageAtGo1Scan^2', all.train.data, regressAgeBOO=FALSE, regressSexBOO=FALSE))
  if(TBV=='TRUE'){
    outputPVals <- apply(dataFrame[,colVals], 2, function(x) returnPVal(x ,dataFrame$averageRating, '+df$ageAtGo1Scan+df$sex+df$ageAtGo1Scan^2', all.train.data, regressAgeBOO=FALSE, regressSexBOO=FALSE, regressTBV=TRUE))
  }  
  # Now fdr correct these suckers
  outputPVals.fdr <- p.adjust(outputPVals, method='fdr')
  output <- cbind(names(outputPVals.fdr),as.numeric(unname(outputPVals.fdr)),as.numeric(unname(outputPVals)))
  # Now append the T values to the output
  outputTVals <- apply(dataFrame[,colVals], 2, function(x) returnTVal(x ,dataFrame$averageRating, '+df$ageAtGo1Scan+df$sex+df$ageAtGo1Scan^2', all.train.data, regressAgeBOO=FALSE, regressSexBOO=FALSE))

  output <- cbind(output, as.numeric(unname(outputTVals)))
  return(output)
}
## Load library(s) we will need
install_load('caret', 'lme4')

## Now lets prep the data
## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- merge(raw.lme.data, tbvData, by='bblid')
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
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
fsCTVals <- pvalLoop('mprage_fs_ct', all.train.data)
jlfCTVals <- pvalLoop('mprage_jlf_ct', all.train.data)
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
fsVolVals <- pvalLoop('mprage_fs_vol', all.train.data, TBV=TRUE)
jlfVOLVals <- pvalLoop('mprage_jlf_vol', all.train.data, TBV=TRUE)

## Now I need to export these in a manner that will make it easy to create a brain picture =D 
write.csv(fsCTVals, 'fsSigQAPROIct.csv', quote=F)
write.csv(jlfCTVals, 'jlfSigQAPROIct.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmd.csv', quote=F)
write.csv(fsVolVals, 'fsSigQAPROIvol.csv', quote=F)
write.csv(jlfVOLVals, 'jlfSigQAPROIvol.csv', quote=F)

# Now we need to create our itksnap color things 
positiveValues <- jlfCTVals[which(as.numeric(jlfCTVals[,3])<0.05&as.numeric(jlfCTVals[,4])>0),]
positiveValues <- cbind(positiveValues, rank(as.numeric(positiveValues[,4])))
positiveValues <- cbind(positiveValues, qnorm(as.numeric(positiveValues[,2])))
write.csv(positiveValues, 'jlfSigQAPROIctPos.csv', quote=F)
negativeValues <- jlfCTVals[which(jlfCTVals[, 3] < 0.05 & jlfCTVals[,4] < 0 ), ]
negativeValues <- cbind(negativeValues, rank(as.numeric(negativeValues[,4])))
negativeValues <- cbind(negativeValues, qnorm(as.numeric(negativeValues[,2])))
write.csv(negativeValues, 'jlfSigQAPROIctNeg.csv', quote=F)
tmp <- returnHeatMapITKSnapVals(positiveValues[,6], hiColor='red', lowColor='yellow')
write.table(x = tmp, file = "./testpos.txt", sep = "\t", quote = F, row.names = F, col.names = F)
tmp <- returnHeatMapITKSnapVals(negativeValues[, 4], hiColor='light blue', lowColor='blue')
write.table(x = tmp, file = "./testneg.txt", sep = "\t", quote = F, row.names = F, col.names = F)
