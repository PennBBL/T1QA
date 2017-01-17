# AFGR June 8 2016
# this script is going to be used to produce a csv with the values needed for the figure 10 mediation triangle image
# The values will include 
# The coefficient between age and the compositie wap value (age ~ qap)
# The coefficient between the compotise qap value and mean fs ct (meanct ~ age + qap)
# and the difference between the coffieicnts w and w/o includeing the comp qap (mean ct ~ age) & (mean ct ~ age + qap)
# also the sobel score value


## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

# Now work with the data
## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data#[index,]
validationData <- raw.lme.data#[-index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
allow.new.levels=T, type='response')

## Now merge our scaled data values with the original data values
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables
all.train.data$meanCT <- apply(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], 1, mean)
tmp <- read.csv('/home/adrose/qapQA/data/averageGMD.csv')
all.train.data <- merge(all.train.data, tmp, by=c('bblid', 'scanid'))
rm(tmp)
all.train.data$meanVOL <- apply(all.train.data[,2630:2727], 1, sum)
all.train.data$modeRating <- apply(all.train.data[,2978:2980], 1, Mode)
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
    allow.new.levels=T, type='response')

tmp <- all.train.data
all.train.data <- tmp[index,]
all.valid.data <- tmp[-index,]
rm(tmp)

# Now try exploring the mediation with the psych::mediate
trainMed <- psych::mediate(x="meanCT", y="ageAtGo1Scan", m="oneVsTwoOutcome", data=all.train.data)
validMed <- psych::mediate(x="meanCT", y="ageAtGo1Scan", m="oneVsTwoOutcome", data=all.valid.data)

# Now do the same thing with the lavaan package
library(lavaan)
# Start with the train data
Y <- scale(all.train.data$meanCT)
X <- scale(all.train.data$ageAtGo1Scan)
M <- scale(all.train.data$oneVsTwoOutcome)
Data <- data.frame(X = X, Y=Y, M=M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = Data, se="bootstrap", bootstrap=10000)
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
boot.fit <- parameterEstimates(fit, boot.ci.type="perc",level=0.95, ci=TRUE,standardized = TRUE)
boot.fit

# Now move on to the valid data
Y <- scale(all.valid.data$meanCT)
X <- scale(all.valid.data$ageAtGo1Scan)
M <- scale(all.valid.data$oneVsTwoOutcome)
Data <- data.frame(X = X, Y=Y, M=M)
model <- ' # direct effect
Y ~ c*X
# mediator
M ~ a*X
Y ~ b*M
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)
'
fit <- sem(model, data = Data, se="bootstrap", bootstrap=1000)
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
boot.fit <- parameterEstimates(fit, boot.ci.type="perc",level=0.95, ci=TRUE,standardized = TRUE)
boot.fit
