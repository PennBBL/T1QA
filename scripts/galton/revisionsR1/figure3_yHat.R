# AFGR June 6 2016
# This script is going to be used
# to showcase the differences of age amongst our 
# rating metrics for the qap paper
# across the training and testing data sets

## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)
# load library(s)
install_load('ggplot2', 'lme4', 'car', 'visreg', 'scales', 'MASS')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- raw.lme.data[which(raw.lme.data$rawAverageRating>=1),]
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 
manualQAData$age <- (manualQAData$ageAtGo1Scan / 12)

# Now create our model outcome column for both 0Vs !0 & 1 vs 2 in both data sets
trainingData$variable <- 'ratingNULL'
validationData$variable <- 'ratingNULL'
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,allow.new.levels=T, type='response')
validationData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=validationData, allow.new.levels=T, type='response')


# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now prepare our values
bg2.vals.train <- summarySE(data=all.train.data, measurevar='oneVsTwoOutcome', groupvars='sex')
bg2.vals.train$Dataset <- rep('Training', nrow(bg2.vals.train))
bg2.vals.valid <- summarySE(data=all.valid.data, measurevar='oneVsTwoOutcome', groupvars='sex')
bg2.vals.valid$Dataset <- rep('Validation', nrow(bg2.vals.valid))
bg2.vals <- rbind(bg2.vals.train, bg2.vals.valid)
bg2.vals$Dataset <- factor(bg2.vals$Dataset)
bg2.vals$sex <- c('Male', 'Female', 'Male', 'Female')

# Now lets plot our values
# Grab a p value from a t.test
pValue <- t.test(all.train.data$oneVsTwoOutcome ~ all.train.data$sex)
#pValue <- wilcox.test(all.train.data$oneVsTwoOutcome ~ all.train.data$sex)
bg1 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Training'),], aes(x=factor(sex), y=as.numeric(as.character(oneVsTwoOutcome)), group=Dataset)) +
                geom_bar(stat='identity', position=position_dodge(), width=.5) + 
                labs(title='Training', x='Sex', y='Quantification Model Outcome\n(mean value)') +
                theme_bw() + 
                coord_cartesian(ylim=c(.8,1)) +
                       geom_errorbar(aes(ymin=as.numeric(as.character(oneVsTwoOutcome))-se, 
                                         ymax=as.numeric(as.character(oneVsTwoOutcome))+se), 
                width = .1, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text=element_text(size=20),
                axis.title=element_text(size=30),
                strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                title=element_text(size=30)) + 
		geom_path(aes(x=factor(sex), y=c(.95,.95))) +
		geom_path(aes(x=factor(sex)[1], y=c(.95, .92))) +
		geom_path(aes(x=factor(sex)[2], y=c(.94, .95))) +
		geom_text(aes(x=factor(sex)[1], y=.97), label='***',angle=90, size=10) +
		scale_y_continuous(limits=c(0, 1),
                           breaks=round(seq(.8, 1, .2), digits=2), oob=rescale_none) +
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.001", hjust=c(2.1), vjust=c(1.1), size=8, parse=T)
		

pValue <- t.test(all.valid.data$oneVsTwoOutcome ~ all.valid.data$sex)
#pValue <- wilcox.test(all.valid.data$oneVsTwoOutcome ~ all.valid.data$sex)
bg2 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Validation'),], aes(x=factor(sex), y=as.numeric(as.character(oneVsTwoOutcome)), group=Dataset)) +
                geom_bar(stat='identity', position=position_dodge(), width=.5) + 
                labs(title='Validation', x='Sex', y='Quantification Model Outcome\n(mean value)') +
                theme_bw() +
                coord_cartesian(ylim=c(.8,1)) +
                       geom_errorbar(aes(ymin=as.numeric(as.character(oneVsTwoOutcome))-se, 
                                         ymax=as.numeric(as.character(oneVsTwoOutcome))+se), 
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
		geom_path(aes(x=factor(sex), y=c(.95,.95))) +
		geom_path(aes(x=factor(sex)[1], y=c(.95, .91))) +
		geom_path(aes(x=factor(sex)[2], y=c(.94, .95))) +
		geom_text(aes(x=factor(sex)[1], y=.97), label='*',angle=90, size=10) +
		scale_y_continuous(limits=c(0, 1),
                           breaks=round(seq(.8, 1, .2), digits=2), oob=rescale_none) +
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.05", hjust=c(2.1), vjust=c(1.1), size=8, parse=T)


# Now build our models to show geneerl age trends
corVal <- cor(all.train.data$oneVsTwoOutcome, all.train.data$age, method='spearman')
corSig <- cor.test(all.train.data$oneVsTwoOutcome, all.train.data$age, method='spearman')$p.value
corText1 <- expression("Spearman's"~rho == .17)
corText2 <- paste("p < 0.001")
mod1 <- ggplot(all.train.data, aes(x=oneVsTwoOutcome, y=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(ylim=c(7,18)) +
   labs(title='', x='Manual Quality Rating (mean value)', y='Age') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30)) +
   scale_x_continuous(breaks=c(0,.25,.5,.75,1)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


corVal <- cor(all.valid.data$oneVsTwoOutcome, all.valid.data$age, method='spearman')
corSig <- cor.test(all.valid.data$oneVsTwoOutcome, all.valid.data$age, method='spearman')$p.value
corText1 <- expression("Spearman's"~rho == paste(0.20))
corText2 <- paste("p < 0.001")
mod2 <- ggplot(all.valid.data, aes(x=oneVsTwoOutcome, y=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(ylim=c(7,18)) +
   labs(title='', x='Manual Quality Rating (mean value)', y='Age') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30),
    axis.title.y=element_text(size=20, color='white'),
    axis.text.y=element_text(size=30, color='white'),
    axis.ticks.y=element_blank()) +
    scale_x_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) + 
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)

png('figure3-demographicsvs1vs2QAPPaper.png', width=16, height=16, units='in', res=300)
multiplot(bg1, mod1, bg2, mod2, cols=2)
dev.off()

# Now build a lm model in the training data
m1 <- lm(oneVsTwoOutcome ~ age + sex, data = all.train.data)
sigValsTrain <- Anova(m1)

# Now do the same for the validation data
m1 <- lm(oneVsTwoOutcome ~ age + sex, data = all.valid.data)
sigValsValid <- Anova(m1)
