# AFGR June 6 2016
# This script is going to be used
# to showcase the differences of age amongst our 
# rating metrics for the qap paper
# across the training and testing data sets

## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret', 'ggplot2')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 
manualQAData$age <- (manualQAData$ageAtGo1Scan / 12)

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now prepare our values
bg1.vals.train <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='age')
bg1.vals.train$Dataset <- rep('Train', nrow(bg1.vals.train))
bg1.vals.valid <- summarySE(data=all.valid.data, groupvars='averageRating', measurevar='age')
bg1.vals.valid$Dataset <- rep('Validation', nrow(bg1.vals.valid))
bg1.vals <- rbind(bg1.vals.train, bg1.vals.valid)
bg1.vals$Dataset <- factor(bg1.vals$Dataset)

bg2.vals.train <- summarySE(data=all.train.data, measurevar='averageRating', groupvars='sex')
bg2.vals.train$Dataset <- rep('Train', nrow(bg2.vals.train))
bg2.vals.valid <- summarySE(data=all.valid.data, measurevar='averageRating', groupvars='sex')
bg2.vals.valid$Dataset <- rep('Validation', nrow(bg2.vals.valid))
bg2.vals <- rbind(bg2.vals.train, bg2.vals.valid)
bg2.vals$Dataset <- factor(bg2.vals$Dataset)
bg2.vals$sex <- c('Male', 'Female', 'Male', 'Female')

# Now lets plot our values
bg1 <- ggplot(bg1.vals, aes(x=factor(averageRating), y=as.numeric(as.character(age)), fill=Dataset, group=Dataset)) + 
                geom_bar(stat='identity', position=position_dodge(), size=.1, aes(fill=Dataset)) + 
                labs(title='Mean Age vs Mean Quality Rating', x='Mean Quality Rating', y='Mean Age') + 
                theme_bw() + 
                coord_cartesian(ylim=c(10,16)) + 
                geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(age))-se, ymax=as.numeric(as.character(age))+se), 
                       width = .2, position=position_dodge(.9)) + 
                theme(legend.position="none") 

bg2 <- ggplot(bg2.vals, aes(x=factor(sex), y=as.numeric(as.character(averageRating)), fill=Dataset, group=Dataset)) + 
                geom_bar(stat='identity', position=position_dodge(), size=.1, aes(fill=Dataset)) + 
                labs(title='Sex vs Average Quality Rating', x='Sex', y='Mean Age') + 
                theme_bw() + 
                coord_cartesian(ylim=c(1.5,2)) + 
                geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se, 
                                         ymax=as.numeric(as.character(averageRating))+se), 
                       width = .2, position=position_dodge(.9))

pdf('demographicsvsRatingQAPPaperFigure3.pdf', width=16, height=10)
multiplot(bg1, bg2, cols=2)
dev.off()



