# AFGR June 13 2016
# This script is going to be used to plot the p cor between the 1 vs 2 model and our average imaging metrics 
## Load the data
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)

## Declare any functions
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

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
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'grid', 'gridExtra')

# Now split data into raw and traning
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])

# Now produce our data sets
raw.lme.data.test <- raw.lme.data[-index,]
raw.lme.data <- raw.lme.data[index,]

# Produce our figures for the training data set
load("/home/adrose/qapQA/data/eulerModels/zero/zeroNotZeroTraining.RData")
raw.lme.data$variable <- "ratingNULL"
raw.lme.data$zeroVsNotZero <- predict(m1, newdata=raw.lme.data, allow.new.levels=T, type='response')
raw.lme.data <- raw.lme.data[complete.cases(raw.lme.data$zeroVsNotZero),]
roc.tmp <- roc(averageRating.x ~ zeroVsNotZero, data=raw.lme.data)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2),".00", sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2, trainText3, trainText4)
trainZeroPlot <- rocplot.single(pred=raw.lme.data$zeroVsNotZero, grp=raw.lme.data$averageRating.x, title="Training")
trainZeroPlot <- trainZeroPlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8) + 
  theme(axis.text.x=element_text(color='white'), axis.ticks.x=element_blank(), axis.title.x=element_text(color='white'))

# Now produce the testing ROC plot using the training data set
raw.lme.data.test$variable <- "ratingNULL"
raw.lme.data.test$zeroVsNotZero <- predict(m1, newdata=raw.lme.data.test, allow.new.levels=T, type='response')
raw.lme.data.test <- raw.lme.data.test[complete.cases(raw.lme.data.test$zeroVsNotZero),]
roc.tmp <- roc(averageRating.x ~ zeroVsNotZero, data=raw.lme.data.test)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2),".00", sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2, trainText3, trainText4)
testZeroPlot <- rocplot.single(pred=raw.lme.data.test$zeroVsNotZero, grp=raw.lme.data.test$averageRating.x, title="Testing")
testZeroPlot <- testZeroPlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8) + 
  theme(axis.text.x=element_text(color='white'), axis.ticks.x=element_blank(), axis.title.x=element_text(color='white'), axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())

# Now do the validation data using the training model???
all.mgi.data$variable <- "ratingNULL"
all.mgi.data$zeroVsNotZero <- predict(m1, newdata=all.mgi.data, allow.new.levels=T, type='response')
all.mgi.data <- all.mgi.data[complete.cases(all.mgi.data$mean_euler),]
all.mgi.data$averageRating.x <- 1
all.mgi.data$averageRating.x[which(all.mgi.data$averageRating == 0)] <- 0
roc.tmp <- roc(averageRating.x ~ zeroVsNotZero, data=all.mgi.data)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2, trainText3, trainText4)
validZeroPlot <- rocplot.single(pred=all.mgi.data$zeroVsNotZero, grp=all.mgi.data$averageRating.x, title="Validation")
validZeroPlot <- validZeroPlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8) + 
  theme(axis.text.x=element_text(color='white'), axis.ticks.x=element_blank(), axis.title.x=element_text(color='white'), axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())

# Now produce all of the 1 vs 2 plots
# Start by producing the data
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
trainText <- c(trainText1, trainText2, trainText3, trainText4)
trainOnePlot <- rocplot.single(pred=raw.lme.data$oneVsTwo, grp=raw.lme.data$averageRating.x, title="")
trainOnePlot <- trainOnePlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8)

# Now do the testing data set
raw.lme.data.test$variable <- "ratingNULL"
raw.lme.data.test$oneVsTwo <- predict(m1, newdata=raw.lme.data.test, allow.new.levels=T, type='response')
raw.lme.data.test <- raw.lme.data.test[complete.cases(raw.lme.data.test$oneVsTwo),]
roc.tmp <- roc(averageRating.x ~ oneVsTwo, data=raw.lme.data.test)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2, trainText3, trainText4)
testOnePlot <- rocplot.single(pred=raw.lme.data.test$oneVsTwo, grp=raw.lme.data.test$averageRating.x, title="")
testOnePlot <- testOnePlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8)  + theme(axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())

# Now do the validation data set
all.mgi.data <- all.mgi.data[which(all.mgi.data$averageRating >=1),]
all.mgi.data$averageRating[all.mgi.data$averageRating>1.5] <- 2
all.mgi.data$oneVsTwo <- predict(m1, newdata=all.mgi.data, allow.new.levels=T, type='response')
roc.tmp <- roc(averageRating ~ oneVsTwo, data=all.mgi.data)
trainText1 <- paste("Classification Accuracy = ", round(coords(roc.tmp, 'best', ret='accuracy'), digits=2))
trainText2 <- paste("AUC =  ", round(auc(roc.tmp), digits=2), sep='')
trainText3 <- paste("PPV = ", round(coords(roc.tmp, 'best', ret='ppv'), digits=2), sep='')
trainText4 <- paste("NPV = ", round(coords(roc.tmp, 'best', ret='npv'), digits=2), sep='')
trainText <- c(trainText1, trainText2, trainText3, trainText4)
validOnePlot <- rocplot.single(pred=all.mgi.data$oneVsTwo, grp=all.mgi.data$averageRating, title="")
validOnePlot <- validOnePlot + annotate("text", x=c(Inf, Inf, Inf, Inf), y=c(-Inf, -Inf, -Inf, -Inf), label=trainText, vjust=c(-1,-2.2, -3.4, -4.6), hjust="inward", size=8) + theme(axis.title.y=element_text(color='white'), axis.text.y=element_text(color='white'), axis.ticks.y=element_blank())

# Now create our output
png('figure7-rocCurves.png', width=18, height=12, units='in', res=300)
multiplot(trainZeroPlot, trainOnePlot, testZeroPlot, testOnePlot, validZeroPlot, validOnePlot, cols=3)
dev.off()


