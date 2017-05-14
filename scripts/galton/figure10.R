## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

## Create a function to get our ggplot2 colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
graph.colors <- gg_color_hue(5)

## Now load the library(s)
install_load('caret', 'ggplot2', 'scales')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data#[index,]
validationData <- raw.lme.data#[-index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')

## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
tmp <- cbind(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], all.train.data[,grep('mprage_jlf_vol', names(all.train.data))])
# Now trim non cortical regions
tmp <- tmp[,-seq(99,129)]
meanCT <- NULL
for(i in seq(1, nrow(tmp))){
  tmpVal <- weighted.mean(x=tmp[i,1:98], w=tmp[i,99:196])
  meanCT <- append(meanCT, tmpVal)
}
tmp <- read.csv('/home/adrose/qapQA/data/averageGMD.csv')
all.train.data <- merge(all.train.data, tmp, by=c('bblid', 'scanid'))
rm(tmp)
all.train.data$meanVOL <- apply(all.train.data[,2623:2720], 1, sum)

# Now create our scaled age and age squared values
all.train.data$age <- scale(all.train.data$ageAtGo1Scan)
all.train.data$ageSq <- (scale(all.train.data$ageAtGo1Scan))^2

# Now produce our age regressed values 
all.train.data$meanCTAgeReg <- lm(meanCT ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanFSCtAgeReg <- rep('NA', nrow(all.train.data))
topindex <- as.numeric(names(lm(bh.meanthickness ~ age + ageSq + sex, data=all.train.data)$residuals))
all.train.data$meanFSCtAgeReg[topindex] <- as.numeric(lm(bh.meanthickness ~ age + ageSq + sex, data=all.train.data)$residuals)
all.train.data$meanFSAreaAgeReg <- as.numeric(lm(bh.totalarea ~ age + ageSq + sex, data=all.train.data)$residuals)
all.train.data$cortexVolumeAgeReg <- 'NA'
all.train.data$cortexVolumeAgeReg[topindex] <-  as.numeric(residuals(lm(CortexVol ~ age + ageSq + sex, data=all.train.data)))
all.train.data$cortexVolumeAgeReg <- as.numeric(all.train.data$cortexVolumeAgeReg)

# Now do the age regressed quality metrics
all.train.data$pcaslAgeReg <- residuals(lm(aslEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$idemoAgeReg <- residuals(lm(idemoEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$nbackAgeReg <- residuals(lm(nbackEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$restAgeReg <- residuals(lm(restEpi10qaMeanrelrms ~ age + ageSq + sex, data=all.train.data, na.action=na.exclude))
all.train.data$oneVsTwoOutcomeAgeReg <- lm(oneVsTwoOutcome ~ age + ageSq + sex, data=all.train.data)$residuals


# Now split into our train and validation data sets
all.data.freeze <- all.train.data
all.train.data <- all.data.freeze[index,]
# Now make sure we grab the intersection between subjectw tih all imaging data
# Now make sure we have subjects with all data
motionCols <- grep('Meanrelrms', names(all.train.data))
all.train.data <- all.train.data[complete.cases(all.train.data[,motionCols]),]
# Now do the same for the validation data set
all.valid.data <- all.data.freeze[-index,]
# Now make sure we have subjects with all data
all.valid.data <- all.valid.data[complete.cases(all.valid.data[,motionCols]),]

# Now get the motion cols
motionCols <- grep('Meanrelrms', names(all.train.data))
motionCols <- append(motionCols, grep('Outcome', names(all.train.data)))
motionCols <- append(motionCols, grep('rawAverageRating.y', names(all.train.data)))


# Now I need to make the time in scanner vs mean rms plots for train and validation data
cols <- grep('Meanrelrms', names(all.train.data))
colsCorrect <- cols[c(3, 2, 1, 4)]

val1 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[1], na.rm=T)
colnames(val1)[3] <- 'mean'
val2 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[2], na.rm=T)
colnames(val2)[3] <- 'mean'
val3 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[3], na.rm=T)
colnames(val3)[3] <- 'mean'
val4 <- summarySE(data=all.train.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[4], na.rm=T)
colnames(val4)[3] <- 'mean'
motionValues <- rbind(val1, val2, val3, val4)
motionValues$.id <- c('PCASL (2:46)', 'tfMRI 1 (14:51)', 'tfMRI 2 (20:19)', 'rsfMRI (43:01)')
motionValues$.id <- factor(motionValues$.id, levels=c('PCASL (2:46)', 'tfMRI 1 (14:51)', 'tfMRI 2 (20:19)', 'rsfMRI (43:01)'))

# Now plot these values
trainMotion <- ggplot(motionValues, aes(x=.id, y=mean, fill=.id)) +
    geom_bar(stat='identity', position=position_dodge(), size=.1) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
    width = .2, position=position_dodge(.9)) +
    theme_bw() +
    theme(legend.position="none") +
    labs(title='Training', x='Time from T1 Scan (min:sec)', y='Mean Relative Displacement (mm)') +
    theme(text=element_text(size=20), 
    axis.text.x = element_text(angle = 0),
    axis.text.y=element_text(size=20),
    title=element_text(size=30),
    axis.title.x=element_text(size=20)) + 
    scale_fill_manual(name=".id", values=c("PCASL (2:46)"="#F8766D", 
				    "tfMRI 1 (14:51)"="#A3A500",
				    "tfMRI 2 (20:19)"="#00BF7D",
				    "rsfMRI (43:01)"="#00B0F6")) +
    scale_y_continuous(limits=c(0, .2), breaks=round(seq(0, .2, .05), digits=2), oob=rescale_none)


# Now do the validation data

# Now I need to make the time in scanner vs mean rms plots for train and validation data
val1 <- summarySE(data=all.valid.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[1], na.rm=T)
colnames(val1)[3] <- 'mean'
val2 <- summarySE(data=all.valid.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[2], na.rm=T)
colnames(val2)[3] <- 'mean'
val3 <- summarySE(data=all.valid.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[3], na.rm=T)
colnames(val3)[3] <- 'mean'
val4 <- summarySE(data=all.valid.data[,colsCorrect],  measurevar=names(all.train.data[,colsCorrect])[4], na.rm=T)
colnames(val4)[3] <- 'mean'
motionValues <- rbind(val1, val2, val3, val4)
motionValues$.id <- c('PCASL (2:46)', 'tfMRI 1 (14:51)', 'tfMRI 2 (20:19)', 'rsfMRI (43:01)')
motionValues$.id <- factor(motionValues$.id, levels=c('PCASL (2:46)', 'tfMRI 1 (14:51)', 'tfMRI 2 (20:19)', 'rsfMRI (43:01)'))

# Now plot the suckers
validMotion <- ggplot(motionValues, aes(x=.id, y=mean, fill=.id)) +
    geom_bar(stat='identity', position=position_dodge(), size=.1) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
    width = .2, position=position_dodge(.9)) +
    theme_bw() +
    theme(legend.position="none") +
    labs(title='Validation', x='Time from T1 Scan (min:sec)', y='Mean Relative Displacement (mm)') +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 0),
    axis.title.y=element_text(color="white", size=20),
    axis.text.y=element_text(size=20, color="white"), 
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(color="white"),
    title=element_text(size=30),
    axis.title.x=element_text(size=20)) + 
    scale_fill_manual(name=".id", values=c("PCASL (2:46)"="#F8766D", 
				    "tfMRI 1 (14:51)"="#A3A500",
				    "tfMRI 2 (20:19)"="#00BF7D",
				    "rsfMRI (43:01)"="#00B0F6")) +
    scale_y_continuous(limits=c(0, .2), breaks=round(seq(0, .2, .05), digits=2), oob=rescale_none)

# Now prepare all of the corellation figures down here
all.train.data <- all.data.freeze[index,]
all.train.data <- all.train.data[which(all.train.data$averageRating.x != 0),]
all.train.data <- all.train.data[complete.cases(all.train.data[,motionCols]),]
attach(all.train.data)
# Lets do the training data first
meanValsAgeRegPcaslT <- cbind(cor(meanCTAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, pcaslAgeReg, use='complete', method='spearman'))

meanValsAgeRegIdemoT <- cbind(cor(meanCTAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(cortexVolumeAgeReg, idemoAgeReg, use='complete', method='spearman'))

meanValsAgeRegNbackT <- cbind(cor(meanCTAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, nbackAgeReg, use='complete', method='spearman'))

meanValsAgeRegRestT <- cbind(cor(meanCTAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, restAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwoT <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(cortexVolumeAgeReg, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))


detach(all.train.data)

# Now do the valid data
all.valid.data <- all.data.freeze[-index,]
all.valid.data <- all.valid.data[which(all.valid.data$averageRating.x!=0),]
attach(all.valid.data)

# Now produce the cor values
meanValsAgeRegPcaslV <- cbind(cor(meanCTAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, pcaslAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           pcaslAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, pcaslAgeReg, use='complete', method='spearman'))

meanValsAgeRegIdemoV <- cbind(cor(meanCTAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           idemoAgeReg, method='spearman', use='complete'),
                                       cor(cortexVolumeAgeReg, idemoAgeReg, use='complete', method='spearman'))

meanValsAgeRegNbackV <- cbind(cor(meanCTAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, nbackAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           nbackAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, nbackAgeReg, use='complete', method='spearman'))

meanValsAgeRegRestV <- cbind(cor(meanCTAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanGMDAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(meanVOLAgeReg, restAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           restAgeReg, use='complete', method='spearman'),
                                       cor(cortexVolumeAgeReg, restAgeReg, use='complete', method='spearman'))

meanValsAgeRegOneVsTwoV <- cbind(cor(meanCTAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanGMDAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(meanVOLAgeReg, oneVsTwoOutcomeAgeReg, method='spearman'),
                                       cor(as.numeric(meanFSCtAgeReg),
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(as.numeric(meanFSAreaAgeReg), 
                                           oneVsTwoOutcomeAgeReg, method='spearman', use='complete'),
                                       cor(cortexVolumeAgeReg, oneVsTwoOutcomeAgeReg, use='complete', method='spearman'))


detach(all.valid.data)

# Now prepare our values to graph
trainData <- rbind(meanValsAgeRegPcaslT, meanValsAgeRegIdemoT, meanValsAgeRegNbackT, meanValsAgeRegRestT, meanValsAgeRegOneVsTwoT)
colnames(trainData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(trainData) <- c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', 'Quantification Model')
trainData <- melt(trainData)
trainData$Var3 <- rep('Training', nrow(trainData))

validData <- rbind(meanValsAgeRegPcaslV, meanValsAgeRegIdemoV, meanValsAgeRegNbackV, meanValsAgeRegRestV, meanValsAgeRegOneVsTwoV)
colnames(validData) <- c('ANTs CT', 'ANTs GMD', 'ANTs Vol', 'FS CT', 'FS Area', 'FS Vol')
rownames(validData) <- c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', 'Quantification Model')
validData <- melt(validData)
validData$Var3 <- rep('Validation', nrow(validData))


# Now combine all of our data
allData <- as.data.frame(rbind(trainData, validData))
allData$Var1 <- factor(allData$Var1, levels=c('PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI', 'Quantification Model'))
allData$Var2 <- factor(allData$Var2, levels=c('FS CT', 'ANTs CT', 'FS Vol', 'ANTs Vol', 'ANTs GMD', 'FS Area'))
allData$value <- abs(as.numeric(as.character(allData$value)))
allData$value[which(allData$Var2=='ANTs Vol' & allData$Var1=='Quantification Model')] <- allData$value[which(allData$Var2=='ANTs Vol' & allData$Var1=='Quantification Model')] * -1
allData$Var3 <- as.factor(allData$Var3)
allData <- allData[-grep('FS Area', allData$Var2),]
allData$Var2 <- factor(allData$Var2, levels=c('ANTs CT','FS CT', 'ANTs Vol', 'FS Vol', 'ANTs GMD'))

# Now get sample sizes so we can compute the minimum r needed for signifiance
trainValue <- getRVal(nrow(all.train.data))
validValue <- getRVal(nrow(all.valid.data))


# Now plot it 
thing1 <- ggplot(allData[which(allData$Var3=='Training'),], aes(x=Var2, y=value, color=Var2, fill=Var1, group=Var1)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1, colour="black") +
  theme(legend.position="right") +
  labs(title='', x='Imaging Measure', y='Motion Estimate and Imaging Metric Correlation') +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90,hjust=1, size=20), 
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position="none") +
  guides(fill = guide_legend(title = "Quality Measure")) +
  scale_y_continuous(limits=c(-.05, .3), 
    breaks=round(seq(-.05, .3, .05), digits=2), oob=rescale_none) + 
 geom_hline(yintercept=trainValue, linetype="longdash", colour="black", size=0.5)

thing2 <- ggplot(allData[which(allData$Var3=='Validation'),], aes(x=Var2, y=value, color=Var2, fill=Var1, group=Var1)) +
  geom_bar(stat='identity', position=position_dodge(), size=.1, colour="black") +
  theme(legend.position="right") +
  labs(title='', x='Imaging Measure', y='Motion Estimate and Imaging Metric Correlation') +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90,hjust=1, size=20), 
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20, color='white'),
        axis.text.y = element_text(color='white', size=20),
        legend.text = element_text(size=20),
        legend.position="none", 
        axis.ticks.y=element_blank()) +
  guides(fill = guide_legend(title = "Quality Measure")) + 
    scale_fill_manual(name="Var1", values=c("PCASL"="#F8766D", 
				    "tfMRI 1"="#A3A500",
				    "tfMRI 2"="#00BF7D",
				    "rsfMRI"="#00B0F6",
				    "Quantification Model" = "#E76BF3")) +
  scale_y_continuous(limits=c(-.05, .3), 
    breaks=round(seq(-.05, .3, .05), digits=2), oob=rescale_none) + 
 geom_hline(yintercept=validValue, linetype="longdash", colour="black", size=0.5)


# Now plot our data
png('figure10-motionCorPlots.png', height=20, width=20, units='in', res=300)
multiplot(trainMotion, thing1, validMotion, thing2, cols=2)
dev.off()
