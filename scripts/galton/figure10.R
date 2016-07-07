# AFGR June 8 2016
# this script is going to be used to produce a csv with the values needed for the figure 10 mediation triangle image
# The values will include 
# The coefficient between age and the compositie wap value (age ~ qap)
# The coefficient between the compotise qap value and mean fs ct (meanct ~ age + qap)
# and the difference between the coffieicnts w and w/o includeing the comp qap (mean ct ~ age) & (mean ct ~ age + qap)
# also the sobel score value


## Load the data
# Now do Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Now load any library(s)
install_load('multilevel', 'mediation')

# Now I need to attach the composite qap value to the mergedQAP df
load('/home/adrose/qapQA/data/tmp6-15/go1Weights.RData')
cols <- c('bg.kurtosis', 'bg.skewness', 'cnr',
          'csf.kurtosis', 'csf.skewness', 'efc',
          'fber', 'fwhm', 'gm.kurtosis',
          'gm.skewness', 'qi1', 'snr', 'wm.kurtosis',
          'wm.skewness')
reg.vals.go <- apply(mergedQAP[cols], 1, function(x) weighted.mean(x, w))
mergedQAP <- cbind(mergedQAP, reg.vals.go)

mergedQAP <- mergedQAP[complete.cases(mergedQAP$mprage_fs_mean_thickness),]

# Create the linear models 
fit1 <- lm(ageAtGo1Scan ~ reg.vals.go, data=mergedQAP)
fit2 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + reg.vals.go, data=mergedQAP)
fit3 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan, data=mergedQAP)


# Now find the output values 
topLeftValue <- summary(fit1)$coefficients[2,1]
topRightValue <- summary(fit2)$coefficients[3,1]
bottomTopValue <- summary(fit3)$coefficients[2,1]
bottomBottomValue <- summary(fit2)$coefficients[2,1]

# Now try and compute a sobel value 
zValue <- sobel(mergedQAP$ageAtGo1Scan, mergedQAP$reg.vals.go, mergedQAP$mprage_fs_mean_thickness)$z.value
bottomBottomBottomValue <- pnorm(zValue) * 2

# Now create a csv with all of these values 
values <- rbind(topLeftValue, topRightValue, bottomTopValue, bottomBottomValue, bottomBottomBottomValue)
words <- rbind(c('Age and Mediator'), c('Mediator and Outcome'), c('Predictor and Outcome no reg'), c('Predictor and Outcome w reg'), c('Sobel Score'))

output <- cbind(words, values)
#write.csv(output, 'figure10QAPPaper.csv', quote=F, row.names=F)

# Now play around with bootstrapping the mediation analysis 
# I am following the tips from this website:
# https://cran.r-project.org/web/packages/mediation/vignettes/mediation.pdf
med.fit <- lm(reg.vals.go ~ ageAtGo1Scan + sex, data=mergedQAP)
out.fit <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + reg.vals.go, data=mergedQAP)
med.out <- mediate(med.fit, out.fit, treat="ageAtGo1Scan", mediator="reg.vals.go", sims=1000)


# Now try exploring the mediation with the psych::mediate
foo <- psych::mediate(y="mprage_fs_mean_thickness", x="ageAtGo1Scan", m="reg.vals.go", data=mergedQAP)

# Now do the same thing with the lavaan package
library(lavaan)
Y <- mergedQAP$mprage_fs_mean_thickness
X <- mergedQAP$ageAtGo1Scan
M <- mergedQAP$reg.vals.go
Data <- data.frame(X = X, Y=Y, M=M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = Data, se="bootstrap", bootstrap=1000)


