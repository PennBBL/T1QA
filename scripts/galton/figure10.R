## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/eulerModels/zero/zeroNotZeroTraining.RData')
zeroModel <- m1
load('/home/adrose/qapQA/data/eulerModels/one/oneVsTwoTraining.RData')
oneModel <- m1
rm(m1)

## Now load the library(s)
install_load('caret', 'ggplot2', 'scales', 'pROC')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
raw.lme.data <- merge(raw.lme.data, mergedQAP, by=intersect(names(raw.lme.data),names(mergedQAP)))

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
raw.lme.data$variable <- 'ratingNULL'
# Now produce our outcomes
raw.lme.data$zeroVsNotZeroOutcome <- predict(zeroModel, newdata=raw.lme.data,
                                               allow.new.levels=T, type='response')
raw.lme.data$oneVsTwoOutcome <- predict(oneModel, newdata=raw.lme.data,
					       allow.new.levels=T, type='response')
raw.lme.data <- raw.lme.data[complete.cases(raw.lme.data$zeroVsNotZeroOutcome),]

# Now get the motion cols
motionCols <- grep('Meanrelrms', names(raw.lme.data))
motionCols <- append(motionCols, grep('zeroVsNotZeroOutcome', names(raw.lme.data)))

# Create the data to plot a survivor
survivorPercents <- matrix(NA, nrow=nrow(raw.lme.data), ncol=length(motionCols))
z <- 1
for(i in motionCols){
  survivorPercents[complete.cases(raw.lme.data[,i]),z] <- raw.lme.data$averageRating[complete.cases(raw.lme.data[,i])]
  z <- z+1
}

# Now produce percents for each
percentVals <- NULL
for(i in 1:length(motionCols)){
    centVals <- table(survivorPercents[,i]) / table(survivorPercents[,5])
    tmpOut <- c(names(raw.lme.data)[motionCols][i] , centVals)
    percentVals <- rbind(percentVals, tmpOut)
}
rownames(percentVals) <- NULL
centValsToPlot <- as.data.frame(percentVals)
centValsToPlot[,2:6] <- apply(centValsToPlot[,2:6], 2, function(x) as.numeric(as.character(x)))
centValsToPlot$V1 <- c("tfMRI 1", "tfMRI 2", "PCASL", "rsfMRI", "T1")
centValsToPlot$V1 <- factor(centValsToPlot$V1, levels=c("T1", "PCASL", "tfMRI 1", "tfMRI 2", "rsfMRI"))
plotData <- melt(centValsToPlot, id.vars='V1')

survivorPlot <- ggplot(plotData, aes(x=V1, y=value, group=variable, color=variable)) +
  geom_line(aes(size=1.5)) +
  labs(title='', x='Sequence', y="% Remaining") +
  theme_bw()+
  theme(legend.position="none",
    axis.text.x = element_text(angle=90, size=16, face='bold'),
    axis.text.y = element_text(angle=0, size=16, face='bold'),
    axis.ticks.x=element_blank(),
    axis.text.y = element_text(size=16, face="bold"),
    axis.title=element_text(size=30,face="bold"))

# Now produce the AUC vals for the training data
raw.lme.data <- raw.lme.data[complete.cases(raw.lme.data[,motionCols]),]
aucVals <- NULL
for(i in motionCols){
  roc.tmp <- roc(raw.lme.data$averageRating.x ~ raw.lme.data[,i])
  auc.tmp <- auc(roc.tmp)
  outTmp <- c(names(raw.lme.data)[i], auc.tmp, '0 vs !0', 'Training')
  aucVals <- rbind(aucVals, outTmp)
}

# Now produce all of the 1 vs 2 values
#raw.lme.data <- data.freeze
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating[raw.lme.data$averageRating<1.5] <- 1
raw.lme.data$averageRating[raw.lme.data$averageRating>1.5] <- 2

# Now grab the motion columns
motionCols <- grep('Meanrelrms', names(raw.lme.data))
motionCols <- append(motionCols, grep('oneVsTwoOutcome', names(raw.lme.data)))

# Now produce the AUC vals
aucVals1 <- NULL
for(i in motionCols){
  roc.tmp <- roc(raw.lme.data$averageRating ~ raw.lme.data[,i])
  auc.tmp <- auc(roc.tmp)
  outTmp <- c(names(raw.lme.data)[i], auc.tmp, '1 vs 2', 'Training')
  aucVals1 <- rbind(aucVals1, outTmp)
}

# Now produce the bar plot!
row.names(aucVals) <- NULL
prettyNames <- c('tfMRI 1', 'tfMRI 2', 'PCASL', 'rsfMRI', 'Euler Model')
aucVals[,1] <- prettyNames
plotData <- as.data.frame(aucVals)
plotData$V2 <- as.numeric(as.character(plotData$V2))
plotData$V1 <- factor(plotData$V1, levels=c('Euler Model', 'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucPlot <- ggplot(plotData, aes(x=V1, y=V2, fill=V1)) + 
                 geom_bar(stat='identity', position=position_dodge(), size=.1) + 
                 labs(title='', x='Motion Estimate', y="AUC") +
                 theme_bw() + 
#facet_grid(V3 ~ V4, space = "free", scales='free_x') +
                 theme(legend.position="none",
                 axis.text.x = element_text(angle=90, size=10, face='bold'),
        	 axis.ticks.x=element_blank(),
                 axis.text.y = element_text(size=16, face="bold"),
                 axis.title=element_text(size=30,face="bold"),
                 strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                 strip.text.x = element_text(size = 16, angle = 0, face="bold")) + 
                 coord_cartesian(ylim=c(.75,1))

# Now produce the second bar plot
row.names(aucVals1) <- NULL
aucVals1[,1] <- prettyNames
plotData <- as.data.frame(aucVals1)
plotData$V2 <- as.numeric(as.character(plotData$V2))
plotData$V1 <- factor(plotData$V1, levels=c('Euler Model', 'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
aucPlot1 <- ggplot(plotData, aes(x=V1, y=V2, fill=V1)) +
geom_bar(stat='identity', position=position_dodge(), size=.1) +
labs(title='', x='Motion Estimate', y="") +
theme_bw() +
#facet_grid(V3 ~ V4, space = "free", scales='free_x') +
theme(legend.position="none",
axis.text.x = element_text(angle=90, size=10, face='bold'),
axis.ticks.x=element_blank(),
axis.text.y = element_text(size=16, face="bold"),
axis.title=element_text(size=30,face="bold"),
strip.text.y = element_text(size = 16, angle = 270, face="bold"),
strip.text.x = element_text(size = 16, angle = 0, face="bold")) +
coord_cartesian(ylim=c(.65,.8))

# Now plot our data
png('figure10-aucCompare.png', height=6, width=18, units='in', res=300)
multiplot(survivorPlot, aucPlot, aucPlot1, cols=3)
dev.off()
