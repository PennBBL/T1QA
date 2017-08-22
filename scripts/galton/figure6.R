## Load the data
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/0vsNot0FinalData.RData')
zeroVsNotZeroModel <- m1
rm(m1)

## Decalre any functions
rocdata <- function(grp, pred){
    # Produces x and y co-ordinates for ROC curve plot
    # Arguments: grp - labels classifying subject status
    #            pred - values of each observation
    # Output: List with 2 components:
    #         roc = data.frame with x and y co-ordinates of plot
    #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
    
    grp <- as.factor(grp)
    if (length(pred) != length(grp)) {
        stop("The number of classifiers must match the number of data points")
    }
    
    if (length(levels(grp)) != 2) {
        stop("There must only be 2 values for the classifier")
    }
    
    cut <- unique(pred)
    tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
    fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
    fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
    tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    roc = data.frame(x = fpr, y = tpr)
    roc <- roc[order(roc$x, roc$y),]
    
    i <- 2:nrow(roc)
    auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
    
    pos <- pred[grp == levels(grp)[2]]
    neg <- pred[grp == levels(grp)[1]]
    q1 <- auc/(2-auc)
    q2 <- (2*auc^2)/(1+auc)
    se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
    ci.upper <- auc + (se.auc * 0.96)
    ci.lower <- auc - (se.auc * 0.96)
    
    se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
    z <- (auc - 0.5)/se.auc.null
    p <- 2*pnorm(-abs(z))
    
    stats <- data.frame (auc = auc,
    p.value = p,
    ci.upper = ci.upper,
    ci.lower = ci.lower
    )
    
    return (list(roc = roc, stats = stats))
}

rocplot.single <- function(grp, pred, title = "ROC Plot", p.value = FALSE){
    require(ggplot2)
    plotdata <- rocdata(grp, pred)

    p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
    geom_line(aes(colour = ""), size=3) +
    geom_abline (intercept = 0, slope = 1) +
    theme_bw() +
    scale_x_continuous("1-Specificity") +
    scale_y_continuous("Sensitivity") +
    scale_colour_manual(values = "#000000") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(1,0)) +
    theme(legend.justification=c(1,0)) +
    theme(legend.title=element_blank(),
    text = element_text(size=30)) + theme(legend.position="none")
    
    return(p)
}

## Load Library(s)
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'grid', 'gridExtra')

## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse1vs2.RData')
#raw.lme.data[,2:33] <- scale(raw.lme.data[,2:33], center=T, scale=T)
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
trainingData <- trainingData[complete.cases(trainingData),]
validationData <- raw.lme.data[-index,]
validationData <- validationData[complete.cases(validationData),]

# Now get the measure vars
measureVars <- names(raw.lme.data)[1:33]
# Now get the id.vars
idVars <- names(raw.lme.data)[35:37]

raw.lme.data <- melt(trainingData, id.vars=measureVars, measure.vars=idVars)
raw.lme.data$value[raw.lme.data$value <= 1] <- 0
raw.lme.data$value[raw.lme.data$value >= 1] <- 1
raw.lme.data.test <- melt(validationData, id.vars=measureVars, measure.vars=idVars)
raw.lme.data.test$value[raw.lme.data.test$value <= 1] <- 0
raw.lme.data.test$value[raw.lme.data.test$value >= 1] <- 1


# Now go through the same step wise process as I do in the 0 vs !0 data
# Now run through each variable of interest and build an ROC curve for it
qapValNamesUse <- qapValNames[-c(1:3, 5:7, 10:12, 14:19, 22:26,33:34)]
qapValNamesUse <- c('bg.kurtosis', 'bg.skewness', 'cnr', 'efc', 'fber', 'qi1', 'snr', 'wm.skewness', 'mean_euler')
aucVals <- NULL
testTmpSet <- validationData
testTmpSet$variable <- 'ratingNULL'
validTmpSet <- all.mgi.data[c(qapValNames, 'averageRating')]
validTmpSet <- validTmpSet[-which(validTmpSet$averageRating==0),]
validTmpSet$averageRating[validTmpSet$averageRating<1.5] <- 0
validTmpSet$averageRating[validTmpSet$averageRating>1.5] <- 1
validTmpSet$variable <- 'ratingNULL'
for(qapVal in qapValNamesUse){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    # NOw train our model in the training data
    m1 <- glmer(model, data=raw.lme.data, family='binomial')
    # Now get our auc vals
    trainAUC <- auc(roc(raw.lme.data$value ~ predict(m1, type='response')))
    # Now get our test value auc
    testAUC <- auc(roc(testTmpSet$averageRating.x ~ predict(m1, type='response', newdata=testTmpSet, allow.new.levels=T)))
    validAUC <- auc(roc(validTmpSet$averageRating ~ predict(m1, type='response', newdata=validTmpSet, allow.new.levels=T)))
    # Now prepare the output
    outputData <- rbind(cbind(trainAUC, 'Training', qapVal), cbind(testAUC, 'Testing', qapVal), cbind(validAUC, 'Validation', qapVal))
    aucVals <- rbind(aucVals, outputData)
    if(qapVal == "mean_euler"){
        save(m1, file="/home/adrose/qapQA/data/eulerModels/one/oneVsTwoTraining.RData")
    }
}
aucValsAll <- aucVals
aucValsAll <- cbind(aucVals, rep('Training', 27))


# Now create our data frame to plot
aucVals <- as.data.frame(aucValsAll)
levels(aucVals$V2) <- c('Training', 'Testing', 'Validation')
levels(aucVals$V4) <- c('Training', 'Testing', 'Validation')
aucVals$trainAUC <- as.numeric(as.character(aucVals$trainAUC))
aucVals$BG <- 0
aucVals$BG[which(aucVals$V2==aucVals$V4)] <- 1
aucVals$prettyQap <- rep(rep(c('BG Kurtosis', 'BG Skewness', 'CNR', 'EFC', 'FBER', 'QI1', 'SNR', 'WM Skewness', 'Mean Euler'), each=3), 1)
aucVals$prettyQap <- factor(aucVals$prettyQap, levels=c('QI1', 'EFC', 'WM Skewness', 'SNR', 'CNR', 'FBER', 'BG Skewness', 'BG Kurtosis', 'Mean Euler'))
aucValPlot <- ggplot(aucVals, aes(x=prettyQap, y=trainAUC)) +
  geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
  coord_cartesian(ylim=c(.5,.9)) +
  facet_grid(. ~ V2, scales="free", space="free_x") +
  theme(axis.text.x = element_text(angle=90,hjust=1, size=20),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(size=30),
    text = element_text(size=30),
    panel.margin = unit(1, "lines")) +
  ggtitle("") +
  xlab("") +
  ylab("AUC")

# Now produce the ROC curves
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
load('/home/adrose/qapQA/data/foldsToUse1vs2.RData')
index <- unlist(folds[1])
raw.lme.data.test <- raw.lme.data[-index,]
raw.lme.data <- raw.lme.data[index,]

# Now produce the 1 vs 2 model
load("/home/adrose/qapQA/data/eulerModels/one/oneVsTwoTraining.RData")
raw.lme.data$variable <- "ratingNULL"
raw.lme.data$oneVsTwo <- predict(m1, newdata=raw.lme.data, allow.new.levels=T, type='response')
raw.lme.data <- raw.lme.data[complete.cases(raw.lme.data$oneVsTwo),]
roc.tmp <- roc(averageRating.x ~ oneVsTwo, data=raw.lme.data)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2),"1", sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2)#, trainText3, trainText4)
trainOnePlot <- rocplot.single(pred=raw.lme.data$oneVsTwo, grp=raw.lme.data$averageRating.x, title="")
trainOnePlot <- trainOnePlot + annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=trainText, vjust=c(-3.4,-4.6), hjust="inward", size=8)

# Now do the testing data set
raw.lme.data.test$variable <- "ratingNULL"
raw.lme.data.test$oneVsTwo <- predict(m1, newdata=raw.lme.data.test, allow.new.levels=T, type='response')
raw.lme.data.test <- raw.lme.data.test[complete.cases(raw.lme.data.test$oneVsTwo),]
roc.tmp <- roc(averageRating.x ~ oneVsTwo, data=raw.lme.data.test)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2)#, trainText3, trainText4)
testOnePlot <- rocplot.single(pred=raw.lme.data.test$oneVsTwo, grp=raw.lme.data.test$averageRating.x, title="")
testOnePlot <- testOnePlot + annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=trainText, vjust=c(-3.4,-4.6), hjust="inward", size=8)  + theme(axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())

# Now do the validation data set
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
all.mgi.data$variable <- "ratingNULL"
all.mgi.data <- all.mgi.data[which(all.mgi.data$averageRating >=1),]
all.mgi.data$averageRating[all.mgi.data$averageRating>1.5] <- 2
all.mgi.data$oneVsTwo <- predict(m1, newdata=all.mgi.data, allow.new.levels=T, type='response')
roc.tmp <- roc(averageRating ~ oneVsTwo, data=all.mgi.data)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2)#, trainText3, trainText4)
validOnePlot <- rocplot.single(pred=all.mgi.data$oneVsTwo, grp=all.mgi.data$averageRating, title="")
validOnePlot <- validOnePlot + annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=trainText, vjust=c(-3.4,-4.6), hjust="inward", size=8) + theme(axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())


png('figure6-AUCAcrossTrain.png', width=20, height=16, units='in', res=300)
grid.arrange(aucValPlot, trainOnePlot, testOnePlot, validOnePlot, ncol = 3, layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4)))
dev.off()
