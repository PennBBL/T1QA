# AFGR November 6th 2016
# This script is going to be used to produce the qap paper table # 3
# This table will consit of the sensitivity and specificity of the 1 vs 2 model


## Load the data
source("/home/adrose/T1QA/scripts/galton/loadMgiData.R")
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

# Now load any library(s)
install_load('ggplot2', 'caret', 'pROC')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
#raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
raw.lme.data$value <- raw.lme.data$averageRating.x
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now create our train roc curve
all.train.data$variable <- rep('ratingNULL', nrow(all.train.data))
trainOutcome <- all.train.data$mean_euler#predict(oneVsTwoModel, newdata=all.train.data,
#allow.new.levels=T, type='response')
trainValues <- all.train.data$averageRating.x
roc.train <- roc(trainValues ~ trainOutcome)

# Now do our validation data
all.valid.data$variable <- rep('ratingNULL', nrow(all.valid.data))
validOutcome <- all.valid.data$mean_euler#predict(oneVsTwoModel, newdata=all.valid.data,
#allow.new.levels=T, type='response')
validValues <- all.valid.data$averageRating.x
roc.valid <- roc(validValues ~ validOutcome)

# Now MGI data
testOutcome <- all.mgi.data$mean_euler
testValues <- all.mgi.data$averageRating
testValues[which(testValues>1)] <- 1
roc.test <- roc(testValues ~ testOutcome, controls=)

# Now prepare our table's values
output.train <- coords(roc.train, 'best', ret=c("threshold", "specificity", "sensitivity", "accuracy"))
output.valid <- coords(roc.valid, output.train[1], ret=c("threshold", "specificity", "sensitivity", "accuracy"))
output.test <- coords(roc.test, output.train[1], ret=c("threshold", "specificity", "sensitivity", "accuracy"))
# Now create our table

output <- rbind(output.train, output.valid, output.test)
rownames(output) <- c('Training', 'Internal Testing', 'External Testing')

# Now write our csv
write.csv(output, 'table3-useabilityROCMetrics.csv', quote=F)

# Now produce the table 4 values
output.train <- coords(roc.train, 'best', ret=c("threshold", "specificity", "sensitivity", "accuracy"))
output.valid <- coords(roc.valid, 'best', ret=c("threshold", "specificity", "sensitivity", "accuracy"))
output.test <- coords(roc.test, 'best', ret=c("threshold", "specificity", "sensitivity", "accuracy"))
output <- rbind(output.train, output.valid, output.test)
rownames(output) <- c('Training', 'Internal Testing', 'External Testing')
write.csv(output, 'table4-useabilityInSetROCMetrics.csv', quote=F)
