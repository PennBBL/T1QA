## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

## Now load the library(s)
install_load('caret', 'corrplot')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now prepare the column and row names
namesNew <- c('tfMRI 1', 'tfMRI 2', 'PCASL', 'rsfMRI')

# Now get the motion cols
motionCols <- grep('Meanrelrms', names(all.train.data))

# Now get the cor values for our train data
trainCorVals <- cor(all.train.data[,motionCols], use='complete')
colnames(trainCorVals) <- namesNew
rownames(trainCorVals) <- namesNew

# Now for the valid data
validCorVals <- cor(all.valid.data[,motionCols], use='complete')
colnames(validCorVals) <- namesNew
rownames(validCorVals) <- namesNew

# Now plot our data
pdf('motionCorPlotsFigure15QAP.pdf', height=10, width=18)
par(mfrow=c(1,2))
corrplot(trainCorVals, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', cl.cex=2, main="Train")
corrplot(validCorVals, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', cl.cex=2, main="Valid")
dev.off()
