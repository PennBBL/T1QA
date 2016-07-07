## Load the data
# Now do Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Now load any library(s)
install_load('ggplot2','scales', 'ppcor', 'visreg')

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

# Now do the same thing although the mediation triangle is a bit different this time
#Here is a brief of the mediation triangle 
#   qap
#age   ct
# We are going to follow the same methodology 
value.one.string <- c('ageAtGo1Scan', 'mprage_fs_mean_thickness')
value.one.p.cor <- pcor(mergedQAP[value.one.string], method='spearman')$estimate[2,1]

# Now while controlling for qap value 
value.two.string <- c('mprage_fs_mean_thickness', 'ageAtGo1Scan', 'reg.vals.go')
value.two.p.cor <- pcor(mergedQAP[value.two.string], method='spearman')$estimate[2,1]

# Now look at relationship between qap and age
value.three.string <- c('ageAtGo1Scan', 'reg.vals.go')
value.three.p.cor <- pcor(mergedQAP[value.three.string], method='spearman')$estimate[2,1]

# Now model the two interactions
fit1 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan + reg.vals.go, data = mergedQAP) 
fit2 <- lm(mprage_fs_mean_thickness ~ ageAtGo1Scan , data = mergedQAP) 

# Now create a bar graph
# First start by prepping the data
thing1 <- rbind(value.one.p.cor, value.two.p.cor)
thing2 <- rbind(c('meanCT ~ ageAtGo1Scan'),c('meanCT ~ ageAtGo1Scan + Comp QAP'))
dataFrameToPlot <- as.data.frame(cbind(thing2, thing1))
output.bg <- ggplot(dataFrameToPlot, aes(x=V1, y=as.numeric(as.character(V2)))) + 
  geom_bar(stat="identity") +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) + 
  labs(title = "Relationship of Age with CT w/ and w/o QAP", x = "Age and CT Relationship", y = "Partial Correllation")


# Now follow the steps from http://quantpsy.org/sobel/sobel.htm
# For the mediation analysis
fit3 <- lm( reg.vals.go ~ ageAtGo1Scan, data=mergedQAP)
fit4 <- lm(mprage_fs_mean_thickness ~ reg.vals.go, data=mergedQAP)
pdf('figure8QapPaper.pdf')
visreg(fit4,"reg.vals.go", xlab="Composite QAP Value", ylab="Mean FS Cortical Thickness")
dev.off()
