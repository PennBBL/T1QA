# AFGR June 4 2016
# This script is going to be used to create figure 2 for the qap paper
# It will produce a 2 x 2 matrix of 4 images these will be 2 corr plots
# and 2 bar graphs, which will show the agreement and total number of
# rating bins for the training and validation data sets

## First lets load our data
source("/home/adrose/T1QA/scripts/galton/loadGo1Data.R")

## Now load any library(s) we need
install_load("corrplot", "caret", "ggplot2", "irr", "grid", "polycor")
set.seed(16)

# Now declare any necassary functions
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

## Now prepare our data
raw.lme.data <- merge(isolatedVars, manualQAData2, by = "bblid")
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x > 1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k = 3, list = T, returnTrain = T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
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
axis.text.x=element_text(size=30, angle=90, color='white'),
axis.text.y=element_text(size=30)) +
guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
title.position = "top", title.hjust = 0.5)) +
theme(legend.position="none") + 
ggtitle(expression(paste("Weighted-", kappa))) + 
coord_equal()

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
plot.title=element_text(size=40, color="white"),
axis.text.x=element_text(size=30, angle=90, color='black'),
axis.text.y=element_text(size=30, color='black')) +
theme(legend.position="none") + 
ggtitle(expression(paste("Validation weighted-", kappa))) + 
coord_equal()



# Now we need to create our bar graphs
dataQaDfTrain <- as.data.frame(table(round(all.train.data$rawAverageRating.x, digits=2)))
dataQaDfTrain <- cbind(dataQaDfTrain, c(0,0,0,1,1.33,1.67,2))
colnames(dataQaDfTrain)[3] <- 'color'
trainBG <- ggplot(dataQaDfTrain, aes(x=Var1, y=Freq, fill=factor(color))) +
geom_bar(stat='identity') +
labs(title='Distribution of Manual Quality Ratings', x='', y='Training') +
geom_text(data=dataQaDfTrain,aes(x=Var1,y=Freq,label=Freq),vjust=0, size=12) +
theme_bw() +
theme(legend.position="none",
axis.text.x=element_text(size=30, color='white'),
axis.text.y=element_text(size=30),
axis.title.y=element_text(size=40, angle=90),
axis.title.x=element_text(size=30),
plot.title=element_text(size=40)) +
scale_y_continuous(limits=c(0,1000), breaks=round(seq(0, 1000, 200), digits=2))

# Now do the validation data
dataQaDfValid <- as.data.frame(table(round(all.valid.data$rawAverageRating.x, digits=2)))
dataQaDfValid <- cbind(dataQaDfValid, c(0,0,0,1,1.33,1.67,2))
colnames(dataQaDfValid)[3] <- 'color'
validBG <- ggplot(dataQaDfValid, aes(x=Var1, y=Freq, fill=factor(color))) +
geom_bar(stat='identity') +
labs(title='', x='Manual Quality Rating (mean value)', y='Validation') +
geom_text(data=dataQaDfValid,aes(x=Var1,y=Freq,label=Freq),vjust=0, size=12) +
theme_bw() +
theme(legend.position="none",
axis.text.x=element_text(size=30),
axis.text.y=element_text(size=30),
axis.title.x=element_text(size=30),
axis.title.y=element_text(size=40, angle=90),
plot.title=element_text(size=40)) +
scale_y_continuous(limits=c(0,500), breaks=round(seq(0, 500, 100), digits=2))


# Now do the polychoric cor's down here
attach(all.train.data)
trainValue <- matrix(NA, nrow=3, ncol=3)
# Start with jason's values
trainValue[1,1] <- polychor(x=ratingJB.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,1] <- polychor(x=ratingJB.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,1] <- polychor(x=ratingJB.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# Now do Kevin's column
trainValue[1,2] <- polychor(x=ratingKS.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,2] <- polychor(x=ratingKS.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,2] <- polychor(x=ratingKS.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# And now prayosha's
trainValue[1,3] <- polychor(x=ratingLV.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,3] <- polychor(x=ratingLV.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,3] <- polychor(x=ratingLV.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# Now fix the column and row names
diag(trainValue) <- 1
colnames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')
rownames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')

# All done with the train data
detach(all.train.data)
trainValueDone <- trainValue

## Now do the validation data
attach(all.valid.data)
trainValue <- matrix(NA, nrow=3, ncol=3)
# Start with jason's values
trainValue[1,1] <- polychor(x=ratingJB.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,1] <- polychor(x=ratingJB.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,1] <- polychor(x=ratingJB.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# Now do Kevin's column
trainValue[1,2] <- polychor(x=ratingKS.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,2] <- polychor(x=ratingKS.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,2] <- polychor(x=ratingKS.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# And now prayosha's
trainValue[1,3] <- polychor(x=ratingLV.x, y=ratingJB.x, ML=TRUE, maxcor=1)
trainValue[2,3] <- polychor(x=ratingLV.x, y=ratingKS.x, ML=TRUE, maxcor=1)
trainValue[3,3] <- polychor(x=ratingLV.x, y=ratingLV.x, ML=TRUE, maxcor=1)

# Now fix the column and row names
diag(trainValue) <- 1
colnames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')
rownames(trainValue) <- c('Rater 1', 'Rater 2', 'Rater 3')

# All done with the validation data set
detach(all.valid.data)
validValueDone <- trainValue


# Now create our cor matrices plots
my_y_title <- expression(paste("Polychoric ", italic("r")))
trainDataPoly <- melt(trainValueDone)
trainCorPoly <- ggplot(data = trainDataPoly, aes(x=Var1, y=Var2, fill=value)) +
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
axis.text.x=element_text(size=30, angle=90, color='white'),
axis.text.y=element_text(size=30, color='white')) +
guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
title.position = "top", title.hjust = 0.5)) +
theme(legend.position="none") + 
labs(title=my_y_title) + 
coord_equal()

validDataPoly <- melt(validValueDone)
validCorPoly <- ggplot(data = validDataPoly, aes(x=Var1, y=Var2, fill=value)) +
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
plot.title=element_text(size=40, color='white'),
axis.text.x=element_text(size=30, angle=90),
axis.text.y=element_text(size=30, color='white')) +
theme(legend.position="none") + 
labs(title=my_y_title) + 
coord_equal()

foo <- ggplot(data = validDataPoly, aes(x=Var1, y=Var2, fill=value)) +
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
legend.position = c(0, 1),
legend.direction = "horizontal",
legend.key.size = unit(2.5, "cm"),
plot.title=element_text(size=40),
axis.text.x=element_text(size=30, angle=90),
axis.text.y=element_text(size=30, color='white')) +
theme(legend.position="bottom")


# Now create our plot
png('figure2-concordanceAmongstRaters.png', height=16, width=30, units='in', res=300)
#multiplot(trainBG,  trainCor, trainCorPoly, validBG, validCor, validCorPoly, cols=3)
multiplot(trainBG,  validBG, trainCor, validCor, trainCorPoly, validCorPoly, cols=3)
#foo
dev.off()

png('foo.png')
foo
dev.off()

# Now run a repeated effects anova down here
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 

raw.lme.data.train <- melt(trainingData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])
raw.lme.data.valid <- melt(validationData, id.vars=names(raw.lme.data)[1:32], measure.vars=names(raw.lme.data)[34:36])

# Now run the repated effects AOV's
aov.train <- aov(value ~ variable, data=raw.lme.data.train)
aov.valid <- aov(value ~ variable, data=raw.lme.data.valid)


