# AFGR June 6 2016
# This script is going to be used to produce the correllation plots for the QAP project
# This is going to be amongst only the qap metrics of interest these are listed below:
#                            ('CNR', 'EFC', 'FBER', 'FWHM',
#                             'QI1', 'SNR', 'BG Kurtosis', 
#                             'BG Skewness', 'CSF Kurtosis',
#                             'CSF Skewness', 'GM Kurtosis',
#                             'GM Skewness', 'WM Kurtosis',
#                             'WM Skewness') 
# These plots will be seperated across training and validation data sets. 

## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret', 'corrplot')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

# Now create a value which contains the columns of interest from the trainindData
colsOfInterest <- c(4, 7, 8, 11, 17, 18, 23, 24, 25, 26, 27, 28, 31, 32)

# Now fix the names of our metrics of interest -not sure if this is necassary or not-
names(trainingData)[colsOfInterest] <- c('CNR', 'EFC', 'FBER', 'FWHM',
                                         'QI1', 'SNR', 'CSF Kurtosis', 
                                         'CSF Skewness', 'GM Kurtosis',
                                         'GM Skewness', 'WM Kurtosis',
                                         'WM Skewness', 'BG Kurtosis',
                                         'BG Skewness') 
names(validationData)[colsOfInterest] <- c('CNR', 'EFC', 'FBER', 'FWHM',
                                         'QI1', 'SNR', 'CSF Kurtosis', 
                                         'CSF Skewness', 'GM Kurtosis',
                                         'GM Skewness', 'WM Kurtosis',
                                         'WM Skewness', 'BG Kurtosis',
                                         'BG Skewness')

# Now create our cor values
trainCor <- cor(trainingData[,colsOfInterest])
validCor <- cor(validationData[,colsOfInterest])

# Now create our plots
pdf('corPlotsFigure4QAP.pdf', height=10, width=20)
par(mfrow=c(1,2))
corrplot(trainCor, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', cl.cex=2, main="Train")
corrplot(validCor, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', cl.cex=2, main="Valid")
dev.off()



