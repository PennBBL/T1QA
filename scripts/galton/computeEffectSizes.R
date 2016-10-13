# AFGR September 8 2016
# This script is going to be used to explore the effect size of the mutliple 
# image quality metrics I am using these include:
#	1.) Raw average rating
#	2.) 1 vs !0 model
#	3.) 1 vs 2 model outcome
# I am going to have to compare the effect size of the outcome of these measures
# there are obviously multiple ways to do this.
# I am how ever going to try to do this using eta squared (essentially corelation)
# also I am going to try to do a cohens d... I think by predicting 
# I am super unsure about how to do this I think I want to do this though 
# lm(CT ~ rating)$residuals 
# Do ^ for each of my model outcomes and then find the difference between groups


## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalData.RData')
zeroVsNotZeroModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)
load('/home/adrose/qapQA/data/go1LmerModel.RData')
zeroVsNotZeroLMERModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModelLMER.RData')
oneVsTwoModelLMER <- mod8
rm(mod8)

# pCor function
pCorFunction <- function(imageVals, toComputeCorWith, toRegressOut){
  newVals1 <- lm(imageVals ~ toRegressOut)$residuals
  newVals2 <- lm(toComputeCorWith ~ toRegressOut)$residuals
  cor(newVals1, newVals2)
}



# Now work with the data 
## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor')


## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
trainingData$zeroVsNotZeroOutcome <- predict(zeroVsNotZeroModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')
# Now do the 0 vs !0 lmer
trainingData$zeroVsNotZeroOutcomeLMER <- predict(zeroVsNotZeroLMERModel, newdata=trainingData,
                                               allow.new.levels=T)
# Now do the 1 vs 2 lmer 
trainingData$oneVsTwoOutcomeLMER <- predict(oneVsTwoModelLMER, newdata=trainingData,
                                            allow.new.levels=T)


## Now merge our scaled dtaa values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
all.train.data$meanCT <- apply(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], 1, mean)
all.train.data$meanGMD <- apply(all.train.data[,grep('mprage_jlf_gmd', names(all.train.data))], 1, mean)
meanGMDWeighted <- NULL
#for(i in 1:nrow(all.train.data)){
#  meanGMDVal <- weighted.mean(all.train.data[i,grep('mprage_jlf_gmd', names(all.train.data))], #all.train.data[i,grep('mprage_jlf_vol', names(all.train.data))])
#  meanGMDWeighted <- append(meanGMDWeighted, meanGMDVal)
#}
#all.train.data$meanGMDWeighted <- meanGMDWeighted
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, mean)
all.train.data$modeRating <- apply(all.train.data[,2978:2980], 1, Mode)
all.train.data$meanFSArea <- apply(all.train.data[,215:282], 1, function(x) mean(x, na.rm=T))

## Okay we have all of our data points
## Now lets compute some effect sizes 
# Now do age regressed data
all.train.data.train <- all.train.data[index,]
all.train.data.train <- all.train.data.train[which(all.train.data.train$averageRating!=0),]

# Now lets loop through and calculate our p cors
ratingVals <- c('rawAverageRating.y', 'modeRating', 'ratingJB.x',
               'ratingKS.x', 'ratingLV.x', 'zeroVsNotZeroOutcome', 'oneVsTwoOutcome', 'restEpi10qaMeanrelrms')
imagingVals <- c('meanCT', 'meanGMD', 'meanVOL', 'mprage_fs_mean_thickness', 'TotalGrayVol')
outputMatrix <- matrix('NA', nrow=length(imagingVals), ncol=length(ratingVals))
for(colVal in seq(1, length(ratingVals))){
    for(rowVal in seq(1, length(imagingVals))){
        tmpData <- cbind(all.train.data.train[ratingVals[colVal]], all.train.data.train[imagingVals[rowVal]], all.train.data.train['ageAtGo1Scan'], all.train.data.train['sex'])
        tmpData <- tmpData[complete.cases(tmpData),]
        outputMatrix[rowVal, colVal] <- pcor(tmpData, method='spearman')$estimate[1,2]
    }
}
colnames(outputMatrix) <- ratingVals
rownames(outputMatrix) <- imagingVals
write.csv(outputMatrix, 'pCorQualityRegAndImagingMetric-Train.csv', quote=F)

# Now do the CV data
all.train.data.cv <- all.train.data[-index,]
all.train.data.cv <- all.train.data.cv[which(all.train.data.cv$averageRating!=0),]
outputMatrix <- matrix('NA', nrow=length(imagingVals), ncol=length(ratingVals))
for(colVal in seq(1, length(ratingVals))){
    for(rowVal in seq(1, length(imagingVals))){
        tmpData <- cbind(all.train.data.cv[ratingVals[colVal]], all.train.data.cv[imagingVals[rowVal]], all.train.data.cv['ageAtGo1Scan'], all.train.data.cv['sex'])
        tmpData <- tmpData[complete.cases(tmpData),]
        outputMatrix[rowVal, colVal] <- pcor(tmpData, method='spearman')$estimate[1,2]
    }
}
colnames(outputMatrix) <- ratingVals
rownames(outputMatrix) <- imagingVals
write.csv(outputMatrix, 'pCorQualityRegAndImagingMetric-Test.csv', quote=F)
