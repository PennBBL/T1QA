# AFGR June 6 2016
# This script is going to be used to produce the correllation plots for the QAP project
# This is going to be amongst only the qap metrics of interest these are listed below:
#                            ('CNR', 'EFC', 'FBER', 'FWHM',
#                             'QI1', 'SNR', 'BG Kurtosis', 
#                             'BG Skewness', 'CSF Kurtosis',
#                             'CSF Skewness', 'GM Kurtosis',
#                             'GM Skewness', 'WM Kurtosis',
#                             'WM Skewness') 
# These plots will be seperated across training and validation data sets. 

## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret', 'ggplot2', 'grid', 'gridExtra', 'cowplot')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

# Now create a value which contains the columns of interest from the trainindData
colsOfInterest <- c(4, 7, 8, 11, 17, 18, 23, 24, 25, 26, 27, 28, 31, 32)

# Now fix the names of our metrics of interest -not sure if this is necassary or not-
names(trainingData)[colsOfInterest] <- c('CNR', 'EFC', 'FBER', 'FWHM',
                                         'QI1', 'SNR', 'CSF Kurtosis', 
                                         'CSF Skewness', 'GM Kurtosis',
                                         'GM Skewness', 'WM Kurtosis',
                                         'WM Skewness', 'BG Kurtosis',
                                         'BG Skewness') 
names(validationData)[colsOfInterest] <- c('CNR', 'EFC', 'FBER', 'FWHM',
                                         'QI1', 'SNR', 'CSF Kurtosis', 
                                         'CSF Skewness', 'GM Kurtosis',
                                         'GM Skewness', 'WM Kurtosis',
                                         'WM Skewness', 'BG Kurtosis',
                                         'BG Skewness')

# Now create our cor values
trainCor <- cor(trainingData[,colsOfInterest], method='spearman')
validCor <- cor(validationData[,colsOfInterest], method='spearman')

# Now melt them so we can work with ggplot2
trainCor <- melt(trainCor)
validCor <- melt(validCor)

# Now create our ggplot2 variables starting with training
trainPlot <- ggplot(data = trainCor, aes(x=Var1, y=Var2, fill=value)) +
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

validPlot <- ggplot(data = validCor, aes(x=Var1, y=Var2, fill=value)) +
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
      axis.text.y=element_text(size=16, face="bold", color='white')) +
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
      title.position = "top", title.hjust = 0.5)) +
    theme(legend.position="none")


testPlot <- ggplot(data = validCor, aes(x=Var1, y=Var2, fill=value)) +
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
      legend.direction = "horizontal",
      plot.title=element_text(size=24, face="bold"),
      axis.text.x=element_text(size=16, face="bold", angle=90),
      axis.text.y=element_text(size=16, face="bold", color='white')) +
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
      title.position = "top", title.hjust = 0.5)) +
      theme(legend.justification = c(1, 0),
  legend.position = 'right',
  legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 3, barheight = 40,
                title.position = "left", title.hjust = 0.5,
                ticks=FALSE))


# Now create our plots
pdf('figure4-corPlots.pdf', height=10, width=20)
multiplot(trainPlot, validPlot, cols=2)
#testPlot
dev.off()
