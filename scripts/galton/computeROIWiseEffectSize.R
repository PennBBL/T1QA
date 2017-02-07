# AFGR October 24 2016
# This script is going to be used to compute the effect size of the outcome of the 1 vs 2 model on individual ROI values
# cortical thickness and volume values will be being predicted as well as GMD just for the heck of it.
# This will be done in both hemispheres which is also important to note.
# The various steps involved in this process are:
#	0.) Produce age regressed values : may or may not include this
#	1.) Model our individual ROI following this method: lm(individualROI ~ qapValues)
#	2.) Perform FDR correction on all ROI's 


## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
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
  range01 <- function(x){
      # Now make sure we have some standard deviation
      # If no standard deviation return 1
      if( is.na(sd(x)) == 'TRUE'){
        output <- rep(1, length(x))
        return(output)
      }
      else if (sd(x) < 0 ){
        output <- rep(1, length(x))
        return(output)
      }
      else
        (x-min(x))/diff(range(x))
  }
  cRamp <- function(x){
    cols <- colorRamp(c(lowColor, hiColor))(range01(as.numeric(x)))
  }   
  # Output values
  outputValues <- matrix(0, nrow=(length(inputZScores)+1), ncol=8)

  # Now cretae our rgb values
  redValues <- round(cRamp(inputZScores)[,1], digits=0)
  greenValues <- round(cRamp(inputZScores)[,2], digits=0)
  blueValues <- round(cRamp(inputZScores)[,3], digits=0)

  # Now create our index column
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

# Now I need to create a function which will combine my output colormaps 
# The issues with this is that some of the color maps will have the same color values.
# If this is the case I need to replicate the intensity value for this color 
# Actually thinking about this I am going to write a function which will wrap the whole process 
# of deciding which values and directions to apply cut off's to
returnPosNegAndNeuColorScale <- function(outputZScores, colorScaleNeg=c('light blue', 'blue'), colorScalePos=c('yellow', 'red'), colorScaleNeu=c('gray'), sigThreshold=.05){
  # MAKE SURE WE ARE DEALING WITH NUMERICS!!!!
  outputZScores <- as.numeric(as.character(outputZScores))
  
  # First convert our sig threshold into a z score to find our cut off value
  cutOff <- abs(qnorm(sigThreshold))

  # Now we need to make our seperate our data into neutral, positive, and negative values
  # We are going to order these just so it is easier to match the labesl to the output ROI
  # when working with the ouput of this function
  negativeValues <- outputZScores[which(outputZScores < 0 & abs(outputZScores) >= cutOff)]
  negativeValues <- negativeValues[order(negativeValues)]
  positiveValues <- outputZScores[which(outputZScores >= cutOff)]
  positiveValues <- positiveValues[order(positiveValues)]
  neutralValues <- outputZScores[which(abs(outputZScores) < cutOff )]
  neutralValues <- neutralValues[order(neutralValues)]

  # Create our blank label row first
  values <- rep(0, 7)
  blankRow <- append(values, paste('"', 'Clear Label' ,'"', sep=''))

  # Now we need to create our individual color scales 
  #startPoint <- NULL
  output <- blankRow
  if(length(negativeValues) > 0 ){
    negativeColors <- returnHeatMapITKSnapVals(negativeValues, lowColor=colorScaleNeg[1], hiColor=colorScaleNeg[2])[2:(length(negativeValues)+1),]
    #negIndex <- max(as.numeric(as.character(negativeColors[,1])))
    #startPoint <- cbind(startPoint, negIndex)
    output <- rbind(output, negativeColors)
  } 
  if(length(neutralValues) > 0){
    neutralColors <- returnHeatMapITKSnapVals(neutralValues, lowColor=colorScaleNeu[1], hiColor=colorScaleNeu[1])[2:(length(neutralValues)+1),]
    #neuIndex <- max(as.numeric(as.character(neutralColors[,1])))
    #startPoint <- cbind(startPoint, neuIndex)
    output <- rbind(output, neutralColors)
  }
  if(length(positiveValues) > 0 ){
    positiveColors <- returnHeatMapITKSnapVals(positiveValues, lowColor=colorScalePos[1], hiColor=colorScalePos[2])[2:(length(positiveValues)+1),]
    #posIndex <- max(as.numeric(as.character(positiveColors[,1])))
    #startPoint <- cbind(startPoint, posIndex)
    output <- rbind(output, positiveColors)
  } 
  # Now I need to make sure that the index column doesn't have any repeats
  # This will be done by running an an index thorugh the first column
  output[,1] <- seq(0, length(outputZScores)) 
  
  # Now we are all set! just need to return our output
  return(output)
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
  
  # Now append the T values to the output
  outputTVals <- apply(dataFrame[,colVals], 2, function(x) returnTVal(x ,dataFrame$averageRating, '+df$ageAtGo1Scan+df$sex+df$ageAtGo1Scan^2', all.train.data, regressAgeBOO=FALSE, regressSexBOO=FALSE))

  # I need to add some direction to my z scores
  # These directions will come from the t values
  outputZScores <- qnorm(outputPVals.fdr, lower.tail=F) * sign(outputTVals)
  
  # Now return the output
  output <- cbind(names(outputZScores), outputZScores) 
  output <- output[order(as.numeric(output[,2])),] 
  rownames(output) <- NULL
  return(output)
}

## Load library(s) we will need
install_load('caret', 'lme4', 'bda', 'ggplot2')

## Now lets prep the data
## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- merge(raw.lme.data, tbvData, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]
## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
validationData$variable <- rep('ratingNULL', nrow(validationData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
validationData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=validationData,
					       allow.new.levels=T, type='response')
## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')
all.valid.data <- merge(mergedQAP, validationData, by='bblid')

## Now remove the 0 values because they do weird things
## to our ants values 
all.train.data <- all.train.data[which(all.train.data$averageRating!=0),]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating!=0),]
vals <- grep('mprage_jlf_ct', names(all.train.data))
zScoreCT <- NULL
binVals <- NULL
for(i in vals){
  foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
  bar <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[2]
  toAppend <- c(names(all.train.data)[i], foo)
  toAppend2 <- c(names(all.train.data)[i], bar)
  zScoreCT <- rbind(zScoreCT, toAppend)
  binVals <- rbind(binVals, toAppend2)
}
binValsApply <- rep(0, length(vals))
binValsApply[which(p.adjust(binVals[,2], method='fdr')<.05)] <- 1
zScoreCT[,2] <- as.numeric(zScoreCT[,2]) * binValsApply
zScoreCT <- zScoreCT[order(as.numeric(zScoreCT[,2])),]

vals <- grep('mprage_jlf_gmd', names(all.train.data))
# Now rm nonesense ROI's
zScoreGMD <- NULL
binVals <- NULL
for(i in vals[39:136]){
  foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
  toAppend <- c(names(all.train.data)[i], foo)  
  zScoreGMD <- rbind(zScoreGMD, toAppend)
}
zScoreGMD <- zScoreGMD[order(as.numeric(zScoreGMD[,2])),]

# Now create our z scores
jlfCTVals <- pvalLoop('mprage_jlf_ct', all.train.data)
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2629,1)]
jlfVOLVals <- pvalLoop('mprage_jlf_vol', all.train.data, TBV=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(jlfCTVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
ctColors <- returnPosNegAndNeuColorScale(zScoreCT[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
gmdColors <- returnPosNegAndNeuColorScale(jlfGMDVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))
gmdColors <- returnPosNegAndNeuColorScale(zScoreGMD[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))
volColors <- returnPosNegAndNeuColorScale(jlfVOLVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))

# Now we need to create our label into our file which matches our ROI to our label
jlfCTVals <- cbind(zScoreCT, seq(1, nrow(zScoreCT)))
jlfGMDVals <- cbind(zScoreGMD, seq(1, nrow(zScoreGMD)))
jlfVOLVals <- cbind(jlfVOLVals, volColors[2:(dim(jlfVOLVals)[1]+1),1])

# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(volColors, file='volColorScale.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIct.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmd.csv', quote=F)
write.csv(jlfVOLVals, 'jlfSigQAPROIvol.csv', quote=F)


# Now do the FS jazz
fsCTVals <- pvalLoop('mprage_fs_ct', all.train.data)
fsVolVals <- pvalLoop('mprage_fs_vol', all.train.data, TBV=TRUE)
write.csv(fsCTVals, 'fsSigQAPROIct.csv', quote=F)
write.csv(fsVolVals, 'fsSigQAPROIvol.csv', quote=F)


### Now do the same thing for the validation data
static <- all.train.data
all.train.data <- all.valid.data
vals <- grep('mprage_jlf_ct', names(all.train.data))
zScoreCT <- NULL
for(i in vals){
  foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
  toAppend <- c(names(all.train.data)[i], foo)
  zScoreCT <- rbind(zScoreCT, toAppend)
}
zScoreCT <- zScoreCT[order(as.numeric(zScoreCT[,2])),]

vals <- grep('mprage_jlf_gmd', names(all.train.data))
# Now rm nonesense ROI's
zScoreGMD <- NULL
for(i in vals[39:136]){
  foo <- mediation.test(mv=all.train.data$oneVsTwoOutcome, iv=all.train.data$ageAtGo1Scan, dv= all.train.data[,i])$Sobel[1]
  toAppend <- c(names(all.train.data)[i], foo)  
  zScoreGMD <- rbind(zScoreGMD, toAppend)
}
zScoreGMD <- zScoreGMD[order(as.numeric(zScoreGMD[,2])),]

# Now create our z scores
jlfCTVals <- pvalLoop('mprage_jlf_ct', all.train.data)
jlfGMDVals <- pvalLoop('mprage_jlf_gmd', all.train.data)
# Now trim the non cortical regions for our JLF vol regions
tmp <- all.train.data
all.train.data <- all.train.data[,-seq(2592,2629,1)]
jlfVOLVals <- pvalLoop('mprage_jlf_vol', all.train.data, TBV=TRUE)
all.train.data <- tmp
rm(tmp)

## Now create our color values to export to ITK snap
ctColors <- returnPosNegAndNeuColorScale(jlfCTVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
ctColors <- returnPosNegAndNeuColorScale(zScoreCT[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red', 'yellow'))
gmdColors <- returnPosNegAndNeuColorScale(jlfGMDVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))
gmdColors <- returnPosNegAndNeuColorScale(zScoreGMD[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))
volColors <- returnPosNegAndNeuColorScale(jlfVOLVals[,2], colorScalePos=c('blue', 'light blue'), colorScaleNeg=c('red','yellow'))

# Now we need to create our label into our file which matches our ROI to our label
jlfCTVals <- cbind(zScoreCT, seq(1, nrow(zScoreCT)))
jlfGMDVals <- cbind(zScoreGMD, seq(1, nrow(zScoreGMD)))
jlfVOLVals <- cbind(jlfVOLVals, volColors[2:(dim(jlfVOLVals)[1]+1),1])

# Now I need to save these color scales and the other thing
write.table(ctColors, file='ctColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(gmdColors, file='gmdColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.table(volColors, file='volColorScaleValid.txt', sep="\t", quote=F, row.names=F, col.names=F)
write.csv(jlfCTVals, 'jlfSigQAPROIctValid.csv', quote=F)
write.csv(jlfGMDVals, 'jlfSigQAPROIgmdValid.csv', quote=F)
write.csv(jlfVOLVals, 'jlfSigQAPROIvolValid.csv', quote=F)




###
###
###
## Now look at our CT paradox down here
###
###
###

# This section is going to be used to compare the raw age corellations vs the partial corellations of CT values
# This si going to be done for each cortical thickness ROI
all.train.data <- static
vals <- grep('mprage_jlf_ct', names(all.train.data))
rValCT <- NULL
regressedQaulityVals <- lm(oneVsTwoOutcome ~ ageAtGo1Scan + ageAtGo1Scan^2 + sex, data=all.train.data)$residuals
regressedAgeVal <- lm(ageAtGo1Scan ~ sex + oneVsTwoOutcome, data=all.train.data)$residuals
for(i in vals){
    # First we need to create our regressed values
    tmpRegVals <- lm(all.train.data[,i] ~ oneVsTwoOutcome + sex, data=all.train.data)$residuals
    # Now find our cor value and our partial cor value
    corVal <- cor(all.train.data$ageAtGo1Scan, all.train.data[,i])
    pCorVal <- cor(tmpRegVals, regressedAgeVal)
    toAppend <- c(names(all.train.data)[i], corVal, 'Raw')
    toAppend <- rbind(toAppend, c(names(all.train.data)[i], pCorVal, 'Partial'))
    toAppend <- rbind(toAppend, c(names(all.train.data)[i], corVal - pCorVal, 'Diff'))
    rValCT <- rbind(rValCT, toAppend)
}
# Now plot these values
rValDiff <- rValCT[grep('Diff', rValCT[,3]),]
rValCT <- rValCT[-grep('Diff', rValCT[,3]),]
rValCT[,1] <-gsub(rValCT[,1], pattern='mprage_jlf_ct_', replacement='')
rValCT <- as.data.frame(rValCT)
rValCT$V1 <- as.factor(rValCT$V1)
rValCT$V2 <- as.numeric(as.character(rValCT$V2))
rValCT$V3 <- as.factor(rValCT$V3)
tmp <- rValCT
rValCT <- rValCT[seq(1,98),]
barPlot <- ggplot(rValCT, aes(x=V1, y=V2, group=V3, fill=V3)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1) +
theme(legend.position="right") +
labs(title='', x='ROI', y='Cor and PCor BTN CT and Age') +
theme(text = element_text(size=30),
axis.text.x = element_text(angle=90,hjust=1, size=16),
axis.title.x = element_text(size=26),
axis.title.y = element_text(size=26),
legend.text = element_text(size=20))

rValCT <- tmp[seq(99,198),]
barPlot2 <- ggplot(rValCT, aes(x=V1, y=V2, group=V3, fill=V3)) +
geom_bar(stat='identity', position=position_dodge(), size=.1) +
theme(legend.position="right") +
labs(title='', x='ROI', y='Cor and PCor BTN CT and Age') +
theme(text = element_text(size=30),
axis.text.x = element_text(angle=90,hjust=1, size=16),
axis.title.x = element_text(size=26),
axis.title.y = element_text(size=26),
legend.text = element_text(size=20))

rValCT <- rValDiff
rValCT[,1] <-gsub(rValCT[,1], pattern='mprage_jlf_ct_', replacement='')
rValCT <- as.data.frame(rValCT)
rValCT$V1 <- as.factor(rValCT$V1)
rValCT$V2 <- as.numeric(as.character(rValCT$V2))
rValCT$V3 <- as.factor(rValCT$V3)

barPlot3 <- ggplot(rValCT[seq(1,49),], aes(x=V1, y=V2, group=V3, fill=V3)) +
geom_bar(stat='identity', position=position_dodge(), size=.1) +
theme(legend.position="right") +
labs(title='', x='ROI', y='Cor and PCor BTN CT and Age') +
theme(text = element_text(size=30),
axis.text.x = element_text(angle=90,hjust=1, size=16),
axis.title.x = element_text(size=26),
axis.title.y = element_text(size=26),
legend.text = element_text(size=20))


barPlot4 <- ggplot(rValCT[seq(50,98),], aes(x=V1, y=V2, group=V3, fill=V3)) +
geom_bar(stat='identity', position=position_dodge(), size=.1) +
theme(legend.position="right") +
labs(title='', x='ROI', y='Cor and PCor BTN CT and Age') +
theme(text = element_text(size=30),
axis.text.x = element_text(angle=90,hjust=1, size=16),
axis.title.x = element_text(size=26),
axis.title.y = element_text(size=26),
legend.text = element_text(size=20))


pdf('ctCorVsPCor.pdf', height=16, width=24)
barPlot
barPlot2
barPlot3
barPlot4
dev.off()


# Now I need to plot the most and the least effected ROI's
# The most from the diff analysis was the right temporal lobe ("R_TMP")
# The R_TMP saw an increase in developmental effects - i.e. the p cor was farther from zero then the raw cor value
# The area that had the largest decrease in corellation values(cor > pcor) was the L_MOG
pdf('scatterCTPlots.pdf', width=12, height=8)
plot(y=scale(all.train.data$mprage_jlf_ct_R_TMP), x=scale(all.train.data$ageAtGo1Scan), xlab='Z-Score Age', ylab='Z-Score CT', main='Age vs Raw R_TMP CT Values')
newVals <- lm(all.train.data$mprage_jlf_ct_R_TMP ~ all.train.data$oneVsTwoOutcome)$residuals
newAge <- lm(all.train.data$ageAtGo1Scan ~ all.train.data$oneVsTwoOutcome)$residuals
plot(y=scale(newVals), x=scale(newAge), xlab='Z-Score Reg Age', ylab='Z-Score Reg CT', main='Reg Age vs Reg R_TMP CT Values')
plot(y=scale(all.train.data$mprage_jlf_ct_L_MOG), x=scale(all.train.data$ageAtGo1Scan), xlab='Z-Score Age', ylab='Z-Score CT', main='Age vs Raw L_MOG CT Values')
newVals <- lm(all.train.data$mprage_jlf_ct_L_MOG ~ all.train.data$oneVsTwoOutcome)$residuals
plot(y=scale(newVals), x=scale(newAge), xlab='Z-Score Reg Age', ylab='Z-Score Reg CT', main='Reg Age vs Reg L_MOG CT Values')
dev.off()
