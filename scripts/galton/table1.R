# AFGR June 3 2016
# I am going to prepare a csv with the information found below 
#     (N)  (%Female) (mean(sd)-Age)
# Train
# Valid

# The first thing I need to do is prepare the Go1 data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now gather the training data values
train.n <- nrow(all.train.data)
train.female <- length(which(all.train.data$sex==2))/train.n
train.mean.age <- mean(all.train.data$ageAtGo1Scan/12)
train.sd.age <- sd(all.train.data$ageAtGo1Scan/12)
train.output <- cbind(c('Training'), train.n, train.female, train.mean.age, train.sd.age)

# Now valid data
valid.n <- nrow(all.valid.data)
valid.female <- length(which(all.valid.data$sex==2))/valid.n
valid.mean.age <- mean(all.valid.data$ageAtGo1Scan/12)
valid.sd.age <- sd(all.valid.data$ageAtGo1Scan/12)
valid.output <- cbind(c('Validation'), valid.n, valid.female, valid.mean.age, valid.sd.age)


#output <- as.data.frame(rbind(go1.output, mgi.output, go2.output))
output <- as.data.frame(rbind(train.output, valid.output))
colnames(output) <- c('Sample', 'N', '% Female', 'Age Mean', 'Age SD')
write.csv(output, './demographicsQAPPaperTable1.csv', quote=F, row.names=F)





go1.n <- nrow(mergedQAP)
go1.female <- length(which(mergedQAP$sex==2))/go1.n
go1.mean.age <- mean(mergedQAP$ageAtGo1Scan/12)
go1.sd.age <- sd(mergedQAP$ageAtGo1Scan/12)
go1.output <- cbind(c('PNC'), go1.n, go1.female, go1.mean.age, go1.sd.age)
