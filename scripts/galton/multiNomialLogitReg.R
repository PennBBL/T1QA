# AFGR June 8 2016
# This script is going to be used to test a lmer model 
# trying to predict data quality 

## Load the go 1 data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Load Library(s)
install_load('psych', 'caret', 'foreach', 'doParallel', 'lme4', 'foreign', 'MASS', 'Hmisc', 'reshape2', 'nnet')

# Set seed
set.seed(16)

# First create our model
model <- as.formula(value ~ cnr + efc + fber + fwhm + 
               qi1 + snr  + csf.kurtosis + csf.skewness +
               gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness + (1|variable))

# Now prep the training data
tmp <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data <- melt(tmp, id.vars=names(tmp)[1:32], measure.vars=names(tmp)[34:36])
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)

# Now train a model... on all of the data
m1 <- lmer(model, data=raw.lme.data)

# Now prep a data set to check the relationship between our 
# predicted outcome and the actual
outcomeVars <- as.data.frame(scale(isolatedVars[,2:32], center=T, scale=T))
variable <- rep('ratingJB', nrow(outcomeVars))
outcomeVars <- as.data.frame(cbind(outcomeVars, variable))
#outcomeVars <- cbind(outcomeVars,

# Now predict the new variables and check the relationship
outcome <- as.vector(predict(m1, newdata=outcomeVars))
response <- as.numeric(as.character(isolatedVars$averageRating))
cor(outcome, response, method="pearson")
plot(response, outcome)


m <- polr(value ~ cnr + efc + fber + fwhm + 
               qi1 + snr  + csf.kurtosis + csf.skewness +
               gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness, data=outcomeVars, Hess=TRUE)


# Now lets try the multinom function from the nnet package

# First prep the data

model <- as.formula(averageRating ~ cnr + efc + fber + fwhm + 
               qi1 + snr  + csf.kurtosis + csf.skewness +
               gm.kurtosis + gm.skewness +
               wm.kurtosis + wm.skewness + bg.kurtosis +
               bg.skewness)

m2 <- multinom(model, data=isolatedVars)


predict(m2, newdata=mergedQAP, "probs")
