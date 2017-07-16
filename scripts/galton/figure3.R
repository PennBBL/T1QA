# AFGR June 6 2016
# This script is going to be used
# to showcase the differences of age amongst our 
# rating metrics for the qap paper
# across the training and testing data sets

## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret', 'ggplot2', 'lme4', 'car', 'visreg', 'scales', 'MASS')

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
bg2.vals.valid$Dataset <- rep('Testing', nrow(bg2.vals.valid))
bg2.vals.mgi <- summarySE(data=all.mgi.data, measurevar='averageRating', groupvars='Gender')
colnames(bg2.vals.mgi)[1] <- 'sex'
bg2.vals.mgi$Dataset <- 'Validation'
bg2.vals <- rbind(bg2.vals.train, bg2.vals.valid, bg2.vals.mgi)
bg2.vals$Dataset <- factor(bg2.vals$Dataset)
bg2.vals$sex <- c('Male', 'Female', 'Male', 'Female', 'Male', 'Female')

# Now lets plot our values
# Grab a p value from a t.test
pValue <- t.test(all.train.data$averageRating ~ all.train.data$sex)
pValue <- wilcox.test(all.train.data$averageRating ~ all.train.data$sex)
bg1 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Training'),], aes(x=factor(sex), y=as.numeric(as.character(averageRating)), group=Dataset)) + 
                geom_bar(stat='identity', position=position_dodge(), width=.5) + 
                labs(title='Training', x='Sex', y='Mean Manual Quality Rating') +
                theme_bw() + 
                coord_cartesian(ylim=c(1.6,2.1)) + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se, 
                                         ymax=as.numeric(as.character(averageRating))+se), 
                width = .1, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text=element_text(size=20),
                axis.title=element_text(size=30),
                strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                title=element_text(size=30)) + 
		geom_path(aes(x=factor(sex), y=c(2,2))) +
		geom_path(aes(x=factor(sex)[1], y=c(1.9, 2))) +
		geom_path(aes(x=factor(sex)[2], y=c(1.95, 2))) +
		geom_text(aes(x=factor(sex)[1], y=2.05), label='*',angle=90, size=10) +
		scale_y_continuous(limits=c(1.6, 2.1), 
                           breaks=round(seq(1.6, 2.1, .2), digits=2), oob=rescale_none) + 
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.05", hjust=c(2.1), vjust=c(1.1), size=8, parse=T)
		

pValue <- t.test(all.valid.data$averageRating ~ all.valid.data$sex)
pValue <- wilcox.test(all.valid.data$averageRating ~ all.valid.data$sex)
bg2 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Training'),], aes(x=factor(sex), y=as.numeric(as.character(averageRating)), group=Dataset)) +
                geom_bar(stat='identity', position=position_dodge(), width=.5) + 
                labs(title='Testing', x='Sex', y='Manual Quality Rating (mean value)') +
                theme_bw() + 
                       geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se, 
                                         ymax=as.numeric(as.character(averageRating))+se), 
                width = .1, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text.y=element_text(size=20, color='white'),
                axis.title.y=element_text(size=30, color='white'),
                axis.text=element_text(size=20),
                axis.title=element_text(size=30),
		axis.ticks.y=element_blank(),
                strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                title=element_text(size=30)) + 
		geom_path(aes(x=factor(sex), y=c(2,2))) +
		geom_path(aes(x=factor(sex)[1], y=c(1.85, 2))) +
		geom_path(aes(x=factor(sex)[2], y=c(1.95, 2))) +
		geom_text(aes(x=factor(sex)[1], y=2.05), label='***',angle=90, size=10) +
		scale_y_continuous(limits=c(1.6, 2.1), 
                           breaks=round(seq(1.6, 2.1, .2), digits=2), oob=rescale_none) + 
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.001", hjust=c(2), vjust=c(1.1), size=8, parse=T)		
pValue <- t.test(all.mgi.data$averageRating~ all.mgi.data$Gender)
bg3 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Validation'),], aes(x=factor(sex), y=as.numeric(as.character(averageRating)), group=Dataset)) +
geom_bar(stat='identity', position=position_dodge(), width=.5) +
labs(title='Validation', x='Sex', y='Manual Quality Rating (mean value)') +
theme_bw() +
geom_errorbar(aes(ymin=as.numeric(as.character(averageRating))-se,
ymax=as.numeric(as.character(averageRating))+se),
width = .1, position=position_dodge(.9)) +
#facet_grid(Dataset ~ .) +
theme(legend.position="none",
axis.text.y=element_text(size=20, color='white'),
axis.title.y=element_text(size=30, color='white'),
axis.text=element_text(size=20),
axis.title=element_text(size=30),
axis.ticks.y=element_blank(),
strip.text.y = element_text(size = 16, angle = 270, face="bold"),
title=element_text(size=30)) +
geom_path(aes(x=factor(sex), y=c(2,2))) +
geom_path(aes(x=factor(sex)[1], y=c(1.8, 2))) +
geom_path(aes(x=factor(sex)[2], y=c(1.8, 2))) +
geom_text(aes(x=factor(sex)[1], y=2.05), label='',angle=90, size=10) +
scale_y_continuous(limits=c(1.6, 2.1),
breaks=round(seq(1.6, 2.1, .2), digits=2), oob=rescale_none) +
annotate("text", x=c(Inf), y=c(Inf), label="p > 0.9", hjust=c(2), vjust=c(1.1), size=8, parse=T)

# Now build our models to show geneerl age trends
corVal <- cor(all.train.data$rawAverageRating.x, all.train.data$age, method='spearman')
corSig <- cor.test(all.train.data$rawAverageRating.x, all.train.data$age, method='spearman')$p.value
corText1 <- expression(~rho == .15)
corText2 <- paste("p < 0.01")
mod1 <- ggplot(all.train.data, aes(y=rawAverageRating.x, x=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(xlim=c(8,16), ylim=c(1.5,2)) +
   labs(title='', y='Mean Manual Quality Rating', x='Age (years)') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30)) +
   scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


corVal <- cor(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')
corSig <- cor.test(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')$p.value
corText1 <- expression(~rho == paste(0.31))
corText2 <- paste("p < 0.01")
mod2 <- ggplot(all.valid.data, aes(y=rawAverageRating.x, x=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(xlim=c(8,16), ylim=c(1.5,2)) +
   labs(title='', y='Mean Manual Quality Rating', x='Age (years)') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30),
    axis.title.y=element_text(size=20, color='white'),
    axis.text.y=element_text(size=30, color='white'),
    axis.ticks.y=element_blank()) +
    scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)

corVal <- cor(all.mgi.data$rawAverageRating, all.mgi.data$age, method='spearman')
corSig <- cor.test(all.mgi.data$rawAverageRating, all.mgi.data$age, method='spearman')$p.value
corText1 <-expression(~rho == paste(-0.11))
corText2 <- paste("p < 0.1")
mod3 <- ggplot(all.mgi.data, aes(y=rawAverageRating, x=age)) +
geom_smooth(method=lm, color='black') +
theme_bw() +
coord_cartesian(xlim=c(20,80), ylim=c(1.5,2)) +
labs(title='', y='Manual Quality Rating (mean value)', x='Age (years)') +
theme(
axis.text=element_text(size=20),
axis.title=element_text(size=30),
axis.title.y=element_text(size=20, color='white'),
axis.text.y=element_text(size=30, color='white'),
axis.ticks.y=element_blank()) +
scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
scale_x_continuous(breaks=c(20,40,60,80)) +
annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


png('figure3-demographicsvsRatingQAPPaper.png', width=24, height=16, units='in', res=300)
multiplot(bg1, mod1, bg2, mod2, bg3, mod3, cols=3)
dev.off()

# Now build a lme model in the training data 
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(trainingData, id.vars = names(raw.lme.data)[1:32], measure.vars = names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value > 1] <- 1
train.data <- merge(raw.lme.data, all.train.data, by = "bblid")
train.data$age <- as.numeric(as.character(train.data$age))
m1 <- lmer(rawAverageRating.y ~ age + sex + (1|variable), data = train.data)
sigValsTrain <- Anova(m1)

# Now do the same for the validation data
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(validationData, id.vars = names(raw.lme.data)[1:32], measure.vars = names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value > 1] <- 1
train.data <- merge(raw.lme.data, all.valid.data, by = "bblid")
train.data$age <- as.numeric(as.character(train.data$age))
m1 <- lmer(rawAverageRating.y ~ age + sex  + (1|variable), data = train.data)
sigValsValid <- Anova(m1)
