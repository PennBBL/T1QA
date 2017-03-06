## Load the data
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

## Declare any functions
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
    
    if (p.value == TRUE){
        annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
    } else {
        annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95%CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
    }
    
    p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
    geom_line(aes(colour = ""), size=3) +
    geom_abline (intercept = 0, slope = 1) +
    theme_bw() +
    scale_x_continuous("False Positive Rate (1-Specificity)") +
    scale_y_continuous("True Positive Rate (Sensitivity)") +
    scale_colour_manual(labels = annotation, values = "#000000") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(1,0)) +
    theme(legend.justification=c(1,0)) +
    theme(legend.title=element_blank(),
    text = element_text(size=30))
    
    return(p)
}


## Load Library(s)
install_load('pROC', 'ggplot2', 'caret', 'lme4', 'grid', 'gridExtra')

## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data <- raw.lme.data[which(raw.lme.data$averageRating.x!=0),]
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x<1.5] <- 1
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1.5] <- 2
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse1vs2.RData')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,]

raw.lme.data <- melt(trainingData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data$value[raw.lme.data$value <= 1] <- 0
raw.lme.data$value[raw.lme.data$value > 1] <- 1


# Now go through the same step wise process as I do in the 0 vs !0 data
# Now run through each variable of interest and build an ROC curve for it
outcome <- raw.lme.data$value
qapValNames <- qapValNames[-grep('size', qapValNames)]
qapValNames <- qapValNames[-grep('mean', qapValNames)]
qapValNames <- qapValNames[-grep('std', qapValNames)]
qapValNames <- qapValNames[-grep('fwhm_', qapValNames)]
qapValNames <- qapValNames[-grep('hm.', qapValNames)]
qapValNames <- qapValNames[-grep('all.', qapValNames)]
aucVals <- NULL
for(qapVal in qapValNames){
    model <- as.formula(paste("value ~", paste(qapVal), paste("+ (1|variable)")))
    m1 <- glmer(model, data=raw.lme.data, family="binomial")
    predictor <- predict(m1, type='response')
    roc.tmp <- roc(outcome ~ predictor)
    output <- cbind(qapVal, auc(roc.tmp))
    aucVals <- rbind(aucVals, output)
}

aucVals[, 1] <- c("CNR*", "EFC*", "FBER*", "FWHM", "QI1*", "SNR*",
"CSF Kurtosis", "CSF Skewness", "GM Kurtosis", "GM Skewness",
"WM Kurtosis", "WM Skewness*", "BG Kurtosis*", "BG Skewness*")


# Now order and find the AUC heirarchy
aucVals <- as.data.frame(aucVals)
aucVals$V2 <- as.numeric(as.character(aucVals$V2))
aucVals <- aucVals[order(aucVals[,2], decreasing =TRUE),]
aucValsMono <- aucVals
vals <- c(.65, .725)

aucOnevsTwoMonovariate <- ggplot(aucVals, aes(x=reorder(qapVal, -V2), y=V2)) +
geom_bar(stat="identity", width=0.4, position=position_dodge(width=0.5)) +
theme(axis.text.x = element_text(angle=90,hjust=1, size=20),
axis.title.x = element_text(size=30),
axis.title.y = element_text(size=30),
text = element_text(size=30)) +
coord_cartesian(ylim=c(.65,.725)) +
ggtitle("Monovariate AUC Values") +
xlab("Image Quality Metrics") +
ylab("AUC")

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
#all.train.data <- read.csv('/home/adrose/qapQA/data/allTrainData.csv')
all.valid.data <- merge(validationData, manualQAData, by='bblid')
#all.valid.data <- read.csv('/home/adrose/qapQA/data/allValidData.csv')

# Now create our train roc curve
all.train.data$variable <- rep('ratingNULL', nrow(all.train.data))
trainOutcome <- predict(oneVsTwoModel, newdata=all.train.data,
allow.new.levels=T, type='response')
trainValues <- all.train.data$averageRating.x
roc.train <- roc(trainValues ~ trainOutcome)
trainPlot <- rocplot.single(trainValues, trainOutcome, title="Training")

# Now we need to append the accuracy of the graph
trainPlot <- trainPlot + geom_text(data=NULL, x=.62, y=.15, label=paste("AUC        = ", round(auc(roc.train), digits=2), '0', sep=''), size=8) + theme(legend.position="none") +
theme(legend.justification=c(1,0)) +
theme(legend.title=element_blank())

trainPlot <- trainPlot + geom_text(data=NULL, x=.5, y=.05, label=paste("Classification Accuracy = ", round(coords(roc.train, 'best', ret='accuracy'), digits=2), sep=''), size=8) + theme(legend.position="none") +
theme(legend.justification=c(1,0)) +
theme(legend.title=element_blank(),
axis.title=element_text(color='white'))

# Now get the cut off value for the accuracy calucalation for the valid data
cutoff <- coords(roc.train, 'best')[1]

# Now do our validation data
all.valid.data$variable <- rep('ratingNULL', nrow(all.valid.data))
validOutcome <- predict(oneVsTwoModel, newdata=all.valid.data,
allow.new.levels=T, type='response')
validValues <- all.valid.data$averageRating.x
roc.valid <- roc(validValues ~ validOutcome)
validPlot <- rocplot.single(validValues, validOutcome, title="Validation")

# Now append the AUC and accuracy as previously performed
validPlot <- validPlot + geom_text(data=NULL, x=.62, y=.15, label=paste("AUC        =", round(auc(roc.valid), digits=2)),size=8) + theme(legend.position="none") +
theme(legend.justification=c(1,0)) +
theme(legend.title=element_blank())

validPlot <- validPlot + geom_text(data=NULL, x=.5, y=.05, label=paste("Classification Accuracy = ", round(coords(roc.valid, cutoff, ret='accuracy'), digits=2),sep=''),size=8) + theme(legend.position="none") +
theme(legend.justification=c(1,0)) +
theme(legend.title=element_blank())


png('figure6-monovariateAUC1vsNot2-withROCCurves.png', width=21, height=12, units='in', res=300)
#multiplot(aucZerovsNotZeroMonovariate, trainPlot, validPlot, cols=3)
grid.arrange(aucOnevsTwoMonovariate, trainPlot, validPlot, ncol = 2, layout_matrix = cbind(c(1,1), c(2,3)))
dev.off()

