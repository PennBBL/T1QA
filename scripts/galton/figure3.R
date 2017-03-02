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
install_load('caret', 'ggplot2', 'lme4', 'car', 'visreg')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 
manualQAData$age <- (manualQAData$ageAtGo1Scan / 12)

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now prepare our values
bg1.vals.train <- summarySE(data=all.train.data, groupvars='averageRating', measurevar='age')
bg1.vals.train$Dataset <- rep('Training', nrow(bg1.vals.train))
bg1.vals.valid <- summarySE(data=all.valid.data, groupvars='averageRating', measurevar='age')
bg1.vals.valid$Dataset <- rep('Validation', nrow(bg1.vals.valid))
bg1.vals <- rbind(bg1.vals.train, bg1.vals.valid)
bg1.vals$Dataset <- factor(bg1.vals$Dataset)

bg2.vals.train <- summarySE(data=all.train.data, measurevar='averageRating', groupvars='sex')
bg2.vals.train$Dataset <- rep('Training', nrow(bg2.vals.train))
bg2.vals.valid <- summarySE(data=all.valid.data, measurevar='averageRating', groupvars='sex')
bg2.vals.valid$Dataset <- rep('Validation', nrow(bg2.vals.valid))
bg2.vals <- rbind(bg2.vals.train, bg2.vals.valid)
bg2.vals$Dataset <- factor(bg2.vals$Dataset)
bg2.vals$sex <- c('Male', 'Female', 'Male', 'Female')

# Now lets plot our values
bg1 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Training'),], aes(x=factor(sex), y=as.numeric(as.character(averageRating)), group=Dataset)) + 
                geom_bar(stat='identity', position=position_dodge(), size=.1, aes(fill=Dataset)) + 
                labs(title='Training', x='Sex', y='Mean Quality Rating') +
                theme_bw() + 
                coord_cartesian(ylim=c(1.6,2)) + 
                geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se, 
                                         ymax=as.numeric(as.character(averageRating))+se), 
                width = .2, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text=element_text(size=16, face="bold"),
                axis.title=element_text(size=20,face="bold"),
                strip.text.y = element_text(size = 16, angle = 270, face="bold"))

bg2 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Validation'),], aes(x=factor(sex), y=as.numeric(as.character(averageRating)), group=Dataset)) + 
                geom_bar(stat='identity', position=position_dodge(), size=.1, aes(fill=Dataset)) + 
                labs(title='Validation', x='Sex', y='Mean Quality Rating') +
                theme_bw() + 
                coord_cartesian(ylim=c(1.6,2)) + 
                geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se, 
                                         ymax=as.numeric(as.character(averageRating))+se), 
                width = .2, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text.y=element_text(size=16, face="bold", color='white'),
                axis.title.y=element_text(size=20,face="bold", color='white'),
		axis.ticks.y=element_blank(),
                strip.text.y = element_text(size = 16, angle = 270, face="bold")) 

# Now build our models to show geneerl age trends
mod1 <- ggplot(all.train.data, aes(x=rawAverageRating.x, y=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   labs(title='', x='Mean Quality Rating', y='Age')
mod2 <- ggplot(all.train.data, aes(x=rawAverageRating.x, y=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   labs(title='', x='Mean Quality Rating', y='Age') + 
   theme()

png('figure3-demographicsvsRatingQAPPaper.png', width=16, height=10, units='in', res=300)
multiplot(bg1, mod1, bg2, mod2, cols=2)
dev.off()

# Now build a lme model in the training data 
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(trainingData, id.vars = names(raw.lme.data)[1:32], measure.vars = names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value > 1] <- 1
train.data <- merge(raw.lme.data, all.train.data, by = "bblid")
train.data$age <- as.factor(train.data$age)
m1 <- lmer(rawAverageRating.y ~ age + sex +age * sex + (1|variable), data = train.data)
sigValsTrain <- Anova(m1)

# Now do the same for the validation data
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(validationData, id.vars = names(raw.lme.data)[1:32], measure.vars = names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value > 1] <- 1
train.data <- merge(raw.lme.data, all.valid.data, by = "bblid")
train.data$age <- as.factor(train.data$age)
m1 <- lmer(rawAverageRating.y ~ age + sex +age * sex + (1|variable), data = train.data)
sigValsValid <- Anova(m1)
