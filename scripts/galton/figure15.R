## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalData.RData')
zeroVsNotZeroModel <- m1
rm(m1)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)


## Now load the library(s)
install_load('caret', 'ggplot2')

# Now load the models
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])

# Now we need to attach our model outcome values
raw.lme.data$variable <- rep('ratingNULL', nrow(raw.lme.data))
raw.lme.data$zeroVsNotZeroOutcome <- predict(zeroVsNotZeroModel, newdata=raw.lme.data,
    allow.new.levels=T, type='response')
# Now lets do our 1 vs 2 model for everyone
raw.lme.data$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=raw.lme.data,
    allow.new.levels=T, type='response')


trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now get the motion cols
motionCols <- grep('Meanrelrms', names(all.train.data))
motionCols <- append(motionCols, grep('Outcome', names(all.train.data)))
motionCols <- append(motionCols, grep('rawAverageRating.y', names(all.train.data)))

# Now get the cor values for our train data
trainCorVals <- cor(all.train.data[,motionCols], use='complete')
trainCorVals <- melt(trainCorVals)

# Now for the valid data
validCorVals <- cor(all.valid.data[,motionCols], use='complete')
validCorVals <- melt(validCorVals)

# Now create our plots
validCorVals$Var1 <- revalue(validCorVals$Var1, c("aslEpi10qaMeanrelrms"="PCASL",
                        "idemoEpi10qaMeanrelrms"="tfMRI 1",
                        "nbackEpi10qaMeanrelrms"="tfMRI 2",
                        "restEpi10qaMeanrelrms"="rsfMRI",
                        "zeroVsNotZeroOutcome"="0 vs !0 Model",
                        "oneVsTwoOutcome"="1 vs 2 Model",
                        "rawAverageRating.y"="Average Rating"))
validCorVals$Var1 <- factor(validCorVals$Var1, levels=c('Average Rating','0 vs !0 Model', '1 vs 2 Model',
                                               'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
validCorVals$Var2 <- revalue(validCorVals$Var2, c("aslEpi10qaMeanrelrms"="PCASL",
                        "idemoEpi10qaMeanrelrms"="tfMRI 1",
                        "nbackEpi10qaMeanrelrms"="tfMRI 2",
                        "restEpi10qaMeanrelrms"="rsfMRI",
                        "zeroVsNotZeroOutcome"="0 vs !0 Model",
                        "oneVsTwoOutcome"="1 vs 2 Model",
                        "rawAverageRating.y"="Average Rating"))
validCorVals$Var2 <- factor(validCorVals$Var2, levels=c('Average Rating','0 vs !0 Model', '1 vs 2 Model',
                                               'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))

trainCorVals$Var1 <- revalue(trainCorVals$Var1, c("aslEpi10qaMeanrelrms"="PCASL",
"idemoEpi10qaMeanrelrms"="tfMRI 1",
"nbackEpi10qaMeanrelrms"="tfMRI 2",
"restEpi10qaMeanrelrms"="rsfMRI",
"zeroVsNotZeroOutcome"="0 vs !0 Model",
"oneVsTwoOutcome"="1 vs 2 Model",
"rawAverageRating.y"="Average Rating"))
trainCorVals$Var1 <- factor(trainCorVals$Var1, levels=c('Average Rating','0 vs !0 Model', '1 vs 2 Model',
'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))
trainCorVals$Var2 <- revalue(trainCorVals$Var2, c("aslEpi10qaMeanrelrms"="PCASL",
"idemoEpi10qaMeanrelrms"="tfMRI 1",
"nbackEpi10qaMeanrelrms"="tfMRI 2",
"restEpi10qaMeanrelrms"="rsfMRI",
"zeroVsNotZeroOutcome"="0 vs !0 Model",
"oneVsTwoOutcome"="1 vs 2 Model",
"rawAverageRating.y"="Average Rating"))
trainCorVals$Var2 <- factor(trainCorVals$Var2, levels=c('Average Rating','0 vs !0 Model', '1 vs 2 Model',
'PCASL', 'tfMRI 1', 'tfMRI 2', 'rsfMRI'))




trainPlot <- ggplot(data = trainCorVals, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",
    midpoint = 0, limit = c(-1,1), space = "Lab") +
    labs(title='Training') +
    #geom_text(aes(Var2, Var1, label = round(value, digits=2)), color = "black", size = 8) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal",
      plot.title=element_text(size=24, face="bold"),
      axis.text.x=element_text(size=16, face="bold", angle=90),
      axis.text.y=element_text(size=16, face="bold")) +
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
      title.position = "top", title.hjust = 0.5)) +
    theme(legend.position="none")


validPlot <- ggplot(data = validCorVals, aes(x=Var1, y=Var2, fill=value)) +
geom_tile() +
scale_fill_gradient2(low = "red", high = "blue", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab") +
labs(title='Validation') +
#geom_text(aes(Var2, Var1, label = round(value, digits=2)), color = "black", size = 8) +
theme(
axis.title.x = element_blank(),
axis.title.y = element_blank(),
panel.grid.major = element_blank(),
panel.border = element_blank(),
panel.background = element_blank(),
axis.ticks = element_blank(),
legend.justification = c(1, 0),
legend.position = c(0.6, 0.7),
legend.direction = "horizontal",
plot.title=element_text(size=24, face="bold"),
axis.text.x=element_text(size=16, face="bold", angle=90),
axis.text.y=element_text(size=16, face="bold", color="white")) +
guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
title.position = "top", title.hjust = 0.5)) +
theme(legend.position="none")

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
motionValues$.id <- c('2:46', '14:51', '20:19', '43:01')
motionValues$.id <- factor(motionValues$.id, levels=c('2:46', '14:51', '20:19', '43:01'))

# Now plot these values
trainMotion <- ggplot(motionValues, aes(x=.id, y=mean)) +
    geom_bar(stat='identity', position=position_dodge(), size=.1) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
    width = .2, position=position_dodge(.9)) +
    theme_bw() +
    theme(legend.position="none") +
    labs(title='', x='Time in Scanner(min:sec)', y='Mean Rel RMS') +
    coord_cartesian(ylim=c(.1,.15,.2)) +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 0))

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
motionValues$.id <- c('2:46', '14:51', '20:19', '43:01')
motionValues$.id <- factor(motionValues$.id, levels=c('2:46', '14:51', '20:19', '43:01'))

# Now plot the suckers
validMotion <- ggplot(motionValues, aes(x=.id, y=mean)) +
    geom_bar(stat='identity', position=position_dodge(), size=.1) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
    width = .2, position=position_dodge(.9)) +
    theme_bw() +
    theme(legend.position="none") +
    labs(title='', x='Time in Scanner(min:sec)', y='Mean Rel RMS') +
    coord_cartesian(ylim=c(.1,.15,.2)) +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 0),
    axis.title.y=element_text(color="white"))


# Now plot our data
pdf('figure15-motionCorPlots.pdf', height=18, width=18)
multiplot(trainPlot, trainMotion, validPlot, validMotion,cols=2)
dev.off()
