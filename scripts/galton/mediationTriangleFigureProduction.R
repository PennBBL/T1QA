# AFGR June 8 2016
# this script is going to be used to produce a csv with the values needed for the figure 10 mediation triangle image
# The values will include 
# The coefficient between age and the compositie wap value (age ~ qap)
# The coefficient between the compotise qap value and mean fs ct (meanct ~ age + qap)
# and the difference between the coffieicnts w and w/o includeing the comp qap (mean ct ~ age) & (mean ct ~ age + qap)
# also the sobel score value


## Load the data
# Now do Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

# Now load any library(s)
install_load('caret', 'lavaan', 'semPlot')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')

## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')


Y <- all.train.data$mprage_fs_mean_thickness
X <- all.train.data$ageAtGo1Scan
M <- all.train.data$oneVsTwoOutcome
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


pdf('testMediationTriangle.pdf')
semPaths(fit, what='std',title = FALSE, ,edge.label.cex=1.5, residuals = F)
dev.off()
