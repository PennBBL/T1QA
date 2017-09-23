## Load the data
source("/home/adrose/T1QA/scripts/galton/loadMgiData.R")
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

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

# Now remove subjects w/o euler
all.train.data <- all.train.data[complete.cases(all.train.data$mean_euler),]
all.train.data$averageRatingAR <- scale(residuals(lm(averageRating ~ ageAtGo1Scan, data = all.train.data)))
all.valid.data$averageRatingAR <- scale(residuals(lm(averageRating ~ ageAtGo1Scan, data = all.valid.data)))
all.mgi.data$averageRatingAR <- scale(residuals(lm(averageRating ~ age, data = all.mgi.data)))

# Now find our misclassified images 
all.train.data$misClasTrue1Pred0 <- 0
all.train.data$misClasTrue1Pred0[which(all.train.data$averageRating>=1 & all.train.data$mean_euler < -217)] <- 1 
all.train.data$misClasVal <- 0 
all.train.data$misClasVal[which(all.train.data$averageRating>=1 & all.train.data$mean_euler < -217)] <- "False Pos"
all.train.data$misClasVal[which(all.train.data$averageRating>=1 & all.train.data$mean_euler > -217)] <- "True Neg"
all.train.data$misClasVal[which(all.train.data$averageRating<1 & all.train.data$mean_euler > -217)] <- "False Neg" 
all.train.data$misClasVal[which(all.train.data$averageRating<1 & all.train.data$mean_euler < -217)] <- "True Pos" 

# Now test age differences 
trueZero <- all.train.data[which(all.train.data$averageRating < 1),]
falseZero <- all.train.data[which(all.train.data$misClasTrue1Pred0==1),]
trueOne <- all.train.data[which(all.train.data$averageRating>=1 & all.train.data$mean_euler > -217),]

# Now test for differences in mean rating
wilcox.test(trueOne$averageRatingAR, falseZero$averageRatingAR)

# Now measure the valid differences 
all.valid.data$misClasTrue1Pred0 <- 0
all.valid.data$misClasTrue1Pred0[which(all.valid.data$averageRating>=1 & all.valid.data$mean_euler < -217)] <- 1 
trueZero <- all.valid.data[which(all.valid.data$averageRating < 1),]
falseZero <- all.valid.data[which(all.valid.data$misClasTrue1Pred0==1),]
trueOne <- all.valid.data[which(all.valid.data$averageRating>=1 & all.valid.data$mean_euler > -217),]

wilcox.test(trueOne$averageRatingAR, falseZero$averageRatingAR)

# Now the mgi data 
all.mgi.data$misClasTrue1Pred0 <- 0
all.mgi.data$misClasTrue1Pred0[which(all.mgi.data$averageRating>=1 & all.mgi.data$mean_euler < -217)] <- 1 
trueZero <- all.mgi.data[which(all.mgi.data$averageRating < 1),]
falseZero <- all.mgi.data[which(all.mgi.data$misClasTrue1Pred0==1),]
trueOne <- all.mgi.data[which(all.mgi.data$averageRating>=1 & all.mgi.data$mean_euler > -217),]

wilcox.test(trueOne$averageRatingAR, falseZero$averageRatingAR)

pdf('misClasExplore.pdf')
# Now plot age differences 
all.train.data$ageAtGo1Scan <- scale(all.train.data$ageAtGo1Scan)
vals <- summarySE(data=all.train.data, groupvars='misClasVal', measurevar='ageAtGo1Scan')
barPlot <- ggplot(vals,aes(x=factor(misClasVal), y=as.numeric(as.character(ageAtGo1Scan)), fill=factor(misClasVal))) + 
                 geom_bar(stat='identity', position=position_dodge(), size=.1) + 
                 labs(title='', y='Age', x='Predicted Group') +
                 geom_errorbar(aes(ymin=as.numeric(as.character(ageAtGo1Scan))-se, ymax=as.numeric(as.character(ageAtGo1Scan))+se), 
                       width = .1, position=position_dodge(.9)) + 
                 theme_bw() + 
                 theme(
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size=16, face="bold"),
        	 axis.ticks.x=element_blank(),
                 axis.title=element_text(size=30,face="bold"),
                 strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                 strip.text.x = element_text(size = 16, angle = 90, face="bold"),
                 panel.margin = unit(2, "lines")) + scale_fill_grey()

vals <- summarySE(data=all.train.data, groupvars='misClasVal', measurevar='rawAverageRating.x')
barPlot2 <- ggplot(vals,aes(x=factor(misClasVal), y=as.numeric(as.character(rawAverageRating.x)), fill=factor(misClasVal))) + 
                 geom_bar(stat='identity', position=position_dodge(), size=.1) + 
                 labs(title='', y='Average Rating', x='Predicted Group') +
                 geom_errorbar(aes(ymin=as.numeric(as.character(rawAverageRating.x))-se, ymax=as.numeric(as.character(rawAverageRating.x))+se), 
                       width = .1, position=position_dodge(.9)) + 
                 theme_bw() + 
                 theme(
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size=16, face="bold"),
        	 axis.ticks.x=element_blank(),
                 axis.title=element_text(size=30,face="bold"),
                 strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                 strip.text.x = element_text(size = 16, angle = 90, face="bold"),
                 panel.margin = unit(2, "lines")) + scale_fill_grey()

vals <- summarySE(data=all.train.data, groupvars='misClasVal', measurevar='mean_euler')
barPlot3 <- ggplot(vals,aes(x=factor(misClasVal), y=as.numeric(as.character(mean_euler)), fill=factor(misClasVal))) + 
                 geom_bar(stat='identity', position=position_dodge(), size=.1) + 
                 labs(title='', y='Mean Euler', x='Predicted Group') +
                 geom_errorbar(aes(ymin=as.numeric(as.character(mean_euler))-se, ymax=as.numeric(as.character(mean_euler))+se), 
                       width = .1, position=position_dodge(.9)) + 
                 theme_bw() + 
                 theme(
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size=16, face="bold"),
        	 axis.ticks.x=element_blank(),
                 axis.title=element_text(size=30,face="bold"),
                 strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                 strip.text.x = element_text(size = 16, angle = 90, face="bold"),
                 panel.margin = unit(2, "lines")) + scale_fill_grey()

print(barPlot)
print(barPlot2)
print(barPlot3)
dev.off()

nVals <- table(all.train.data$misClasVal, all.train.data$sex)
