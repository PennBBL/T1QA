# AFGR July 13 2016
# This script is going to be used to explore two things
# 1.) rm' the predicted 0's from a glmer and then 
# perform a 1 vs 2 glmer logit model and compare cortical thickness between these groups
# 2.) run an ordinal regression and compare the outcome group's cortical thickness values 
# and then comapre each predicted groups outcome cortical thickness values.


### Start with all Go1 Images here!!!
# First things first - load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Load Library(s)
install_load('psych', 'caret','pROC', 'lme4', 'nnet', 'visreg')

# Set seed
set.seed(16)


#######
#######
#######
##0vs!0
#######
#######
#######

# Cretae a data set with the scaled and centered values
#tmp <- merge(isolatedVars, manualQAData2, by='bblid')
#raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
#raw.lme.data$value[raw.lme.data$value==2] <- 1
#raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x==2] <- 1


# The first thing I am going to do is create a 5 fold validation data set
folds <- createFolds(raw.lme.data$averageRating.x, k=5, list=T, returnTrain=T)

# Now declare the model we are going to use for the glmer
model <- as.formula(averageRating.x ~ cnr + 
               efc + fber + fwhm + 
                qi1 + snr  + csf.kurtosis + csf.skewness + gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now declare the model we are going to use for the glmer
model <- as.formula(averageRating.x ~  
               efc + fber + fwhm + 
                snr  + 
               wm.skewness + bg.kurtosis +
               bg.skewness)


# Now declare the training and validation data sets
index <- as.numeric(unlist(folds[1]))
train <- raw.lme.data[index,]
validate <- raw.lme.data[-index,]

# Now lets train our model
m1 <- glm(model, data=train, family='binomial')

# Now lets validate our traing set 
outcome <- as.vector(predict(m1, newdata=validate, type='response'))
response <- validate$averageRating.x
roc.tmp <- roc(response ~ outcome)
auc(roc.tmp)

# Now lets rm our predicted 0's from the data set 
outcome <- as.vector(predict(m1, newdata=raw.lme.data, type='response'))
response <- raw.lme.data$averageRating.x
roc.tmp <- roc(response ~ outcome)
auc(roc.tmp)
rmThreshold <- unname(coords(roc.tmp, 'best')[1])
#Now I need to rm all of the subjects that are below the threshold
raw.lme.data$newPrediction <- rep('NA', nrow(raw.lme.data))
raw.lme.data$newPrediction[which(outcome>rmThreshold)] <- 1
raw.lme.data$newPrediction[which(outcome<rmThreshold)] <- 0

# Now merge the cortical thickness values and lets comapre our group's average
first.compare.values <- merge(raw.lme.data, mergedQAP, by.x='bblid', by.y='bblid.x')

# Now create a model which predicts cortical thickness via age with group as a regressor 
thicknessModel <- as.formula(paste('mprage_fs_mean_thickness ~  ageAtGo1Scan + newPrediction'))
fit <- lm(thicknessModel, data=first.compare.values)
visreg(fit, "ageAtGo1Scan", by="newPrediction", overlay=TRUE)


#######
#######
#######
##1vs2
#######
#######
#######

# First thing is prep the data set 
one.vs.two.data <- first.compare.values[which(first.compare.values$newPrediction==1),]
one.vs.two.data$newPrediction[which(one.vs.two.data$averageRating!=2)] <- 0

# Now create a training set 
folds <- createFolds(one.vs.two.data$newPrediction, k=5, list=T, returnTrain=T)

# Now declare the training and validation data sets
index <- as.numeric(unlist(folds[1]))
train <- raw.lme.data[index,]
validate <- raw.lme.data[-index,]

# Now lets train our model
m2 <- glm(model, data=train, family='binomial')

# Now lets validate our traing set 
outcome <- as.vector(predict(m2, newdata=validate, type='response'))
response <- validate$newPrediction
roc.tmp <- roc(response ~ outcome)
auc(roc.tmp)

# Now lets rm our predicted 0's from the data set
colnames(one.vs.two.data) <- gsub(pattern='.x', replacement = '', x = colnames(one.vs.two.data), fixed = TRUE) 
outcome <- as.vector(predict(m2, newdata=one.vs.two.data, type='response'))
response <- one.vs.two.data$newPrediction
roc.tmp <- roc(response ~ outcome)
auc(roc.tmp)
rmThreshold <- unname(coords(roc.tmp, 'best')[1])

