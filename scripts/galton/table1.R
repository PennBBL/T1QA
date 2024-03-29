# AFGR June 3 2016
# I am going to prepare a csv with the information found below 
#     (N)  (%Female) (mean(sd)-Age)
# Train
# internal test
# external test

# The first thing I need to do is prepare the Go1 data
source("/home/adrose/T1QA/scripts/galton/loadMgiData.R")
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
all.train.data <- all.train.data[complete.cases(all.train.data$mean_euler),]
all.valid.data <- merge(validationData, manualQAData, by='bblid')
all.valid.data <- all.valid.data[complete.cases(all.valid.data$mean_euler),]
all.mgi.data <- all.mgi.data[complete.cases(all.mgi.data$mean_euler),]

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
valid.output <- cbind(c('Internal Testing'), valid.n, valid.female, valid.mean.age, valid.sd.age)

# Now do MGI
test.n <- nrow(all.mgi.data)
test.female <- length(which(all.mgi.data$Gender==2))/test.n
test.mean.age <- mean(all.mgi.data$age)
test.mean.sd <- sd(all.mgi.data$age)
test.output <- cbind(c('External Testing'), test.n, test.female, test.mean.age, test.mean.sd)

#output <- as.data.frame(rbind(go1.output, mgi.output, go2.output))
output <- as.data.frame(rbind(train.output, valid.output, test.output))
colnames(output) <- c('Sample', 'N', '% Female', 'Age Mean', 'Age SD')
write.csv(output, './table1-demographicsQAPPaper.csv', quote=F, row.names=F)
