# AFGR June 13 2016
# This script is just going to be used to perform a quick mediation effect 
# It is goint o be used to find the relationship between three variables of interest:
# 1.) Manual rating
# 2.) Composite QAP
# 3.) Cortical Thickness


## Load the data
# Now do Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Now load any library(s)
install_load('ggplot2','scales', 'ppcor')

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


# Now lets first find the relationship between manual rating and composite QAP
value.one.string <- c('averageRating', 'reg.vals.go', 'ageAtGo1Scan', 'sex')
value.two.string <- c('averageRating', 'mprage_fs_mean_thickness', 'ageAtGo1Scan', 'sex')
value.three.string <- c('reg.vals.go', 'mprage_fs_mean_thickness', 'ageAtGo1Scan', 'sex')

# Now actually compute the p cors
#value.one.p.cor <- pcor(value.one.string, var(mergedQAP[value.one.string]))
value.one.p.cor <- pcor(mergedQAP[value.one.string], method='spearman')$estimate[2,1]
#value.two.p.cor <- pcor(value.two.string, var(mergedQAP[value.two.string]))
value.two.p.cor <- pcor(mergedQAP[value.two.string], method='spearman')$estimate[2,1]
#value.three.p.cor <- pcor(value.three.string, var(mergedQAP[value.three.string]))
value.three.p.cor <- pcor(mergedQAP[value.three.string], method='spearman')$estimate[2,1]


# Now compute the p cors w/o zero images 
mergedQAP <- mergedQAP[which(mergedQAP$averageRating!=0),]
value.one.p.cor.noz <- pcor(mergedQAP[value.one.string], method='spearman')$estimate[2,1]
value.two.p.cor.noz <- pcor(mergedQAP[value.two.string], method='spearman')$estimate[2,1]
value.three.p.cor.noz <- pcor(mergedQAP[value.three.string], method='spearman')$estimate[2,1]

# Now prepare a data frame for plotting 
vals <- c('AvgRating&RegVals', 'AvgRating&CT', 'RegVals&CT','AvgRating&RegVals', 'AvgRating&CT', 'RegVals&CT')
familys <- c('With 0','With 0','With 0','Without 0','Without 0','Without 0')
values <- c(value.one.p.cor, value.two.p.cor,value.three.p.cor , value.one.p.cor.noz ,value.two.p.cor.noz,value.three.p.cor.noz)  

dataFrameToPlot <- as.data.frame(cbind(vals, familys, values))
dataFrameToPlot$values <- as.numeric(as.character(dataFrameToPlot$values))

# Now plot our data frame
output.bg <- ggplot(dataFrameToPlot, aes(x=vals, y=values)) + 
  geom_bar(stat="identity") +
  facet_grid(. ~ familys, scales="free", space="free_x") + 
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) + 
  labs(title = "Mediation Triangle Partial Spearman Cors", x = "QAP Measures", y = "Partial Correllation")

pdf('mediationTriangleCors.pdf', width=12, height=8)
print(output.bg)
dev.off()


# Now do the raw cors 
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()

# Now load any library(s)
install_load('ggplot2','scales', 'ppcor')

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

# Now compute the cors 
mergedQAP <- mergedQAP[complete.cases(mergedQAP$mprage_fs_mean_thickness),]
value.one.p.cor <- cor(mergedQAP$averageRating, mergedQAP$reg.vals.go,method='spearman')
value.two.p.cor <- cor(mergedQAP$averageRating,mergedQAP$mprage_fs_mean_thickness, method='spearman')
value.three.p.cor <- cor(mergedQAP$reg.vals.go,mergedQAP$mprage_fs_mean_thickness, method='spearman')

# Now rm the 0 images
mergedQAP <- mergedQAP[which(mergedQAP$averageRating!=0),]
value.one.p.cor.noz <- cor(mergedQAP$averageRating, mergedQAP$reg.vals.go,method='spearman')
value.two.p.cor.noz <- cor(mergedQAP$averageRating,mergedQAP$mprage_fs_mean_thickness, method='spearman')
value.three.p.cor.noz <- cor(mergedQAP$reg.vals.go,mergedQAP$mprage_fs_mean_thickness, method='spearman')

# Now prepare a data frame for plotting 
vals <- c('AvgRating&RegVals', 'AvgRating&CT', 'RegVals&CT','AvgRating&RegVals', 'AvgRating&CT', 'RegVals&CT')
familys <- c('With 0','With 0','With 0','Without 0','Without 0','Without 0')
values <- c(value.one.p.cor, value.two.p.cor,value.three.p.cor , value.one.p.cor.noz ,value.two.p.cor.noz,value.three.p.cor.noz)  
dataFrameToPlot <- as.data.frame(cbind(vals, familys, values))
dataFrameToPlot$values <- as.numeric(as.character(dataFrameToPlot$values))

# Now plot our data frame
output.bg <- ggplot(dataFrameToPlot, aes(x=vals, y=values)) + 
  geom_bar(stat="identity") +
  facet_grid(. ~ familys, scales="free", space="free_x") + 
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) + 
  labs(title = "Mediation Triangle Spearman Cors", x = "QAP Measures", y = "Correllation")

pdf('mediationTriangleRawCors.pdf', width=12, height=8)
print(output.bg)
dev.off()
