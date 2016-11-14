# AFGR June 4 2016
# This script is going to be used to create figure 2 for the qap paper
# It will produce a 2 x 2 matrix of 4 images these will be 2 corr plots
# and 2 bar graphs, which will show the agreement and total number of
# rating bins for the training and validation data sets

## First lets load our data
source("/home/adrose/T1QA/scripts/galton/loadGo1Data.R")

## Now load any library(s) we need
install_load("corrplot", "caret", "ggplot2", "irr", "grid")
set.seed(16)

# Now declare any necassary functions
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

## Now prepare our data
raw.lme.data <- merge(isolatedVars, manualQAData2, by = "bblid")
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x > 1] <- 1
folds <- createFolds(raw.lme.data$averageRating.x, k = 3, list = T,
returnTrain = T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index, ]
validationData <- raw.lme.data[-index, ]
all.train.data <- merge(trainingData, manualQAData, by = "bblid")
all.valid.data <- merge(validationData, manualQAData, by = "bblid")

## Now lets produce our ICC values for our training data
attach(all.train.data)
trainValue <- matrix(NA, nrow=3, ncol=3)
# Start with jason's values
trainValue[1,1] <- kappa2(cbind(ratingJB.x, ratingJB.x),weight='squared')$value
trainValue[2,1] <- kappa2(cbind(ratingJB.x, ratingKS.x),weight='squared')$value
trainValue[3,1] <- kappa2(cbind(ratingJB.x, ratingLV.x),weight='squared')$value

# Now do Kevin's column
trainValue[1,2] <- kappa2(cbind(ratingKS.x, ratingJB.x),weight='squared')$value
trainValue[2,2] <- kappa2(cbind(ratingKS.x, ratingKS.x),weight='squared')$value
trainValue[3,2] <- kappa2(cbind(ratingKS.x, ratingLV.x),weight='squared')$value

# And now prayosha's
trainValue[1,3] <- kappa2(cbind(ratingLV.x, ratingJB.x),weight='squared')$value
trainValue[2,3] <- kappa2(cbind(ratingLV.x, ratingKS.x),weight='squared')$value
trainValue[3,3] <- kappa2(cbind(ratingLV.x, ratingLV.x),weight='squared')$value

# Now fix the column and row names
colnames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')
rownames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')

# All done with the train data
detach(all.train.data)
trainValueDone <- trainValue


## Now do the validation data
attach(all.valid.data)
trainValue <- matrix(NA, nrow=3, ncol=3)
# Start with jason's values
trainValue[1,1] <- kappa2(cbind(ratingJB.x, ratingJB.x),weight='squared')$value
trainValue[2,1] <- kappa2(cbind(ratingJB.x, ratingKS.x),weight='squared')$value
trainValue[3,1] <- kappa2(cbind(ratingJB.x, ratingLV.x),weight='squared')$value

# Now do Kevin's column
trainValue[1,2] <- kappa2(cbind(ratingKS.x, ratingJB.x),weight='squared')$value
trainValue[2,2] <- kappa2(cbind(ratingKS.x, ratingKS.x),weight='squared')$value
trainValue[3,2] <- kappa2(cbind(ratingKS.x, ratingLV.x),weight='squared')$value

# And now prayosha's
trainValue[1,3] <- kappa2(cbind(ratingLV.x, ratingJB.x),weight='squared')$value
trainValue[2,3] <- kappa2(cbind(ratingLV.x, ratingKS.x),weight='squared')$value
trainValue[3,3] <- kappa2(cbind(ratingLV.x, ratingLV.x),weight='squared')$value

# Now fix the column and row names
colnames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')
rownames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')

# All done with the validation data set
detach(all.valid.data)
validValueDone <- trainValue

# Now create our cor matrices plots
trainData <- melt(trainValueDone)
trainCor <- ggplot(data = trainData, aes(x=Var1, y=Var2, fill=value)) +
geom_tile() +
scale_fill_gradient2(low = "red", high = "blue", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab") +
geom_text(aes(Var2, Var1, label = round(value, digits=2)), color = "black", size = 16) +
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
plot.title=element_text(size=40),
axis.text.x=element_text(size=30, angle=90),
axis.text.y=element_text(size=30)) +
guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
title.position = "top", title.hjust = 0.5)) +
theme(legend.position="none") +
labs(title='Training')

validData <- melt(validValueDone)
validCor <- ggplot(data = validData, aes(x=Var1, y=Var2, fill=value)) +
geom_tile() +
scale_fill_gradient2(low = "red", high = "blue", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab") +
geom_text(aes(Var2, Var1, label = round(value, digits=2)), color = "black", size = 16) +
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
plot.title=element_text(size=40),
axis.text.x=element_text(size=30, angle=90),
axis.text.y=element_text(size=30)) +
theme(legend.position="none") +
labs(title='Validation')



# Now we need to create our bar graphs
dataQaDfTrain <- as.data.frame(table(round(all.train.data$rawAverageRating.x, digits=2)))
trainBG <- ggplot(dataQaDfTrain, aes(x=Var1, y=Freq, fill=Var1)) +
geom_bar(stat='identity') +
labs(title='', x='Average Quality Rating', y='# Images', size=20) +
geom_text(data=dataQaDfTrain,aes(x=Var1,y=Freq,label=Freq),vjust=0, size=12) +
theme_bw() +
theme(legend.position="none",
axis.text.x=element_text(size=30),
axis.text.y=element_text(size=30),
axis.title.y=element_text(size=30),
axis.title.x=element_text(size=30))


# Now do the validation data
dataQaDfValid <- as.data.frame(table(round(all.valid.data$rawAverageRating.x, digits=2)))
validBG <- ggplot(dataQaDfValid, aes(x=Var1, y=Freq, fill=Var1)) +
geom_bar(stat='identity') +
labs(title='', x='Average Quality Rating', y='# Images') +
geom_text(data=dataQaDfValid,aes(x=Var1,y=Freq,label=Freq),vjust=0, size=12) +
theme_bw() +
theme(legend.position="none",
axis.text.x=element_text(size=30),
axis.text.y=element_text(size=30),
axis.title.x=element_text(size=30),
axis.title.y=element_text(size=30, color='white'))


# Now create our plot
pdf('figure2-concordanceAmongstRaters.pdf', height=20, width=20)
multiplot(trainCor,  trainBG, validCor, validBG, cols=2)
dev.off()
