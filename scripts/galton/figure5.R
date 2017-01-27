# AFGR October 26 2016
# This script is going to be used to plot the differences amongst the training and testing sets 
# in relation to the quantitative variables and their relation to the qualititative manual ratings.
# It is going to plot the 8 values from the outcome of the 1 vs 2 octavariate model
# These include:
#	qi1 wm.skewness cnr bg.kurtosis efc bg.skewness fber snr


## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# Modify summarySE to make it easier to work with
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

# load library(s)
install_load('caret', 'ggplot2')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

# Now lets declare our variables of interest 
varsOfInterest <- c('bg.kurtosis', 'bg.skewness', 'cnr', 'efc', 'fber', 'qi1', 'snr', 'wm.skewness')
prettyNames <- c('BG Kurtosis', 'BG Skewness', 'CNR', 'EFC', 'FBER', 'QI1', 'SNR', 'WM Skewness')

# Now lets create our training values
trainingValues <- NULL 
i <- 1
for(qapVal in varsOfInterest){
  valsToAppend <- summarySE(data=trainingData, measurevar=qapVal, groupvars='averageRating.y')
  qapValue <- rep(prettyNames[i], nrow(valsToAppend))
  Dataset <- rep('Training', nrow(valsToAppend))
  valsToAppend <- cbind(valsToAppend, qapValue, Dataset)
  trainingValues <- rbind(trainingValues, valsToAppend)
  i <- i + 1
}
i <- 1
for(qapVal in varsOfInterest){
  valsToAppend <- summarySE(data=validationData, measurevar=qapVal, groupvars='averageRating.y')
  qapValue <- rep(prettyNames[i], nrow(valsToAppend))
  Dataset <- rep('Validation', nrow(valsToAppend))
  valsToAppend <- cbind(valsToAppend, qapValue, Dataset)
  trainingValues <- rbind(trainingValues, valsToAppend)
  i <- i + 1
}

# Now make our plot
allPlot <- ggplot(trainingValues, 
                 aes(x=factor(averageRating.y), y=as.numeric(as.character(mean)), fill=factor(averageRating.y))) + 
                 geom_bar(stat='identity', position=position_dodge(), size=.1) + 
                 labs(title='', x='Mean Quality Rating', y='Mean Standardized Quality Metric (z-score)') +
                 geom_errorbar(aes(ymin=as.numeric(as.character(mean))-se, ymax=as.numeric(as.character(mean))+se), 
                       width = .1, position=position_dodge(.9)) + 
                 theme_bw() + 
                 facet_grid(Dataset ~ qapValue) + 
                 theme(legend.position="none",
                 axis.text.x = element_text(angle=90,hjust=1, size=16, face="bold"),
                 axis.text.y = element_text(size=16, face="bold"),
                 axis.title=element_text(size=20,face="bold"),
                 strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                 strip.text.x = element_text(size = 16, angle = 90, face="bold"))

png('figure5-qapMetricsVsQCQAPPaper.png', height=12, width=16, units='in', res=300)
allPlot
dev.off()

