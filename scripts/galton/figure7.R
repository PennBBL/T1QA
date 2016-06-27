# AFGR June 13 2016
# This script is going to be used to produce figure 6 for the qap paper.
# Figure 6 will be one bar plot and some statistics for p cor between qap and average rating 
# The formula will be qap ~ rating + age + sex 
# Its going to be one bar plot of p cor values
# One plot of signifiance betwen the value and qap 
# I will be doing this across the whole cohort
# I also need to include motion parameters in this... although I am not sure how that is going to work yet.
# Also one last note I need the ggm package to compute pcor


# First thing is first load the data
source('/home/adrose/qapQA/scripts/loadMgiData.R')
detachAllPackages()
# Declare the mgi data to work with here
mgi.data <- cbind(isolatedVars ,mergedQAP$Gender, mergedQAP$age)
colnames(mgi.data)[34:35] <- c('sex', 'age')

mergedQAP <- mergedQAP.go2
colnames(mergedQAP) <- gsub(pattern='.x', replacement = '', x = colnames(mergedQAP), fixed = TRUE)
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
go2.data <- merge(mergedQAP, manualQAData, by='bblid')

# Declare the go2 data to work with here
go2.data <- cbind(go2.data$bblid, go2.data[qapValNames],go2.data$averageRating,go2.data$sex, go2.data$ageAtGo2Scan)
size.vars <- grep('size', names(go2.data))
go2.data <- go2.data[,-size.vars]
colnames(go2.data)[c(1,33,34,35)] <- c('bblid', 'averageRating', 'sex', 'age')

# Now do Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
go1.data <- cbind(mergedQAP$bblid.x, mergedQAP[qapValNames], mergedQAP$averageRating, mergedQAP$sex, mergedQAP$ageAtGo1Scan)
size.vars <- grep('size', names(go1.data))
go1.data <- go1.data[,-size.vars]
colnames(go1.data)[c(1,33,34,35)] <- c('bblid', 'averageRating', 'sex', 'age')

# Now source library(s)
install_load('ggplot2', 'ggm', 'scales')

# Now combine all of the data sets
# first reorder the columns so all data sets match each other
go1.data <- go1.data[order(names(go1.data))]
mgi.data <- mgi.data[order(names(mgi.data))]
go2.data <- go2.data[order(names(go2.data))]
all.data <- rbind(go1.data, mgi.data, go2.data)

# Now declare the columns of interest to compute p cor for
cols <- names(all.data)[c(8,9,10,13,14,15,16,19,25,26,29,31,34,35)]



# Now attach the combined qap measure
load('/home/adrose/qapQA/data/tmp6-15/go1Weights.RData')
# First do all
reg.vals.go <- apply(all.data[cols], 1, function(x) weighted.mean(x, w))
#all.data <- cbind(all.data, reg.vals.go)
# Now do go1
reg.vals.go <- apply(go1.data[cols], 1, function(x) weighted.mean(x, w))
#go1.data <- cbind(go1.data, reg.vals.go)
# Now mgi
reg.vals.go <- apply(mgi.data[cols], 1, function(x) weighted.mean(x, w))
#mgi.data <- cbind(mgi.data, reg.vals.go)
# Now do go2
reg.vals.go <- apply(go2.data[cols], 1, function(x) weighted.mean(x, w))
#go2.data <- cbind(go2.data, reg.vals.go)


# Now attach the combined qap measure with the mgi data
load('/home/adrose/qapQA/data/tmp6-15/mgiWeights.RData')
# First do all
reg.vals.mg <- apply(all.data[cols], 1, function(x) weighted.mean(x, w))
#all.data <- cbind(all.data, reg.vals.mg)
# Now do go1
reg.vals.mg <- apply(go1.data[cols], 1, function(x) weighted.mean(x, w))
#go1.data <- cbind(go1.data, reg.vals.mg)
# Now mgi
reg.vals.mg <- apply(mgi.data[cols], 1, function(x) weighted.mean(x, w))
#mgi.data <- cbind(mgi.data, reg.vals.mg)
# Now do go2
reg.vals.mg <- apply(go2.data[cols], 1, function(x) weighted.mean(x, w))
#go2.data <- cbind(go2.data, reg.vals.mg)

#cols <- append(cols, 'reg.vals.go')
#cols <- append(cols, 'reg.vals.mg')

# Now compute p cor for all data sets
p.cor.vector <- NULL
for(qapMetricVal in cols){
  p.cor.string <- c('averageRating', qapMetricVal, 'age', 'sex')
  p.cor.val<- pcor(p.cor.string, var(all.data[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)
}

p.cor.data.frame <- as.data.frame(cbind(cols, p.cor.vector))
p.cor.data.frame$p.cor.vector <- as.numeric(as.character(p.cor.data.frame$p.cor.vector))

all.data.bg <- ggplot(p.cor.data.frame, aes(x=cols, y=p.cor.vector, fill=p.cor.vector)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  labs(title = "ALL", x = "QAP Measures", y = "Partial Correllation") + 
  theme(legend.position = 'none')+ 
  scale_y_continuous(limits=c(-.5,.5),oob=rescale_none)


# Now do go1
p.cor.vector <- NULL
for(qapMetricVal in cols){
  p.cor.string <- c('averageRating', qapMetricVal, 'age', 'sex')
  p.cor.val<- pcor(p.cor.string, var(go1.data[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)
}

p.cor.data.frame <- as.data.frame(cbind(cols, p.cor.vector))
p.cor.data.frame$p.cor.vector <- as.numeric(as.character(p.cor.data.frame$p.cor.vector))

# Now find outif the qap value is significant in this equation 
# averageRating ~ qapValue + age + sex
sig.vector <- NULL
for(qapMetricVal in cols){
  lm.formula <- as.formula(paste('averageRating ~age + sex + ', qapMetricVal))
  lm.outcome<- lm(formula = lm.formula, data=go1.data)
  p.value <- summary(lm.outcome)$coefficients[4,4]
  sig.vector <- append(sig.vector, p.value)
}

tmp <- p.cor.data.frame$cols[which(sig.vector < .05)]
asterick.vals <- NULL
for(i in tmp){asterick.vals <- append(asterick.vals,(paste(i, '*', sep='')))}
p.cor.data.frame$cols <- as.character(p.cor.data.frame$cols)
p.cor.data.frame$cols[which(sig.vector < .05)] <- asterick.vals 


go1.data.bg <- ggplot(p.cor.data.frame, aes(x=cols, y=p.cor.vector, fill=p.cor.vector)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  labs(title = "PNC 1", x = "QAP Measures", y = "Partial Correllation") + 
  theme(legend.position = 'none')+ 
  scale_y_continuous(limits=c(-.5,.5),oob=rescale_none)


# Now do go2
p.cor.vector <- NULL
for(qapMetricVal in cols){
  p.cor.string <- c('averageRating', qapMetricVal, 'age', 'sex')
  p.cor.val<- pcor(p.cor.string, var(go2.data[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)
}

p.cor.data.frame <- as.data.frame(cbind(cols, p.cor.vector))
p.cor.data.frame$p.cor.vector <- as.numeric(as.character(p.cor.data.frame$p.cor.vector))

go2.data.bg <- ggplot(p.cor.data.frame, aes(x=cols, y=p.cor.vector, fill=p.cor.vector)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  labs(title = "PNC 2", x = "QAP Measures", y = "Partial Correllation") + 
  theme(legend.position = 'none')+ 
  scale_y_continuous(limits=c(-.5,.5),oob=rescale_none)

# Now do mgi
p.cor.vector <- NULL
for(qapMetricVal in cols){
  p.cor.string <- c('averageRating', qapMetricVal, 'age', 'sex')
  p.cor.val<- pcor(p.cor.string, var(mgi.data[p.cor.string]))
  p.cor.vector <- append(p.cor.vector, p.cor.val)
}

p.cor.data.frame <- as.data.frame(cbind(cols, p.cor.vector))
p.cor.data.frame$p.cor.vector <- as.numeric(as.character(p.cor.data.frame$p.cor.vector))
# Now find outif the qap value is significant in this equation 
# averageRating ~ qapValue + age + sex
sig.vector <- NULL
for(qapMetricVal in cols){
  lm.formula <- as.formula(paste('averageRating ~age + sex + ', qapMetricVal))
  lm.outcome<- lm(formula = lm.formula, data=go1.data)
  p.value <- summary(lm.outcome)$coefficients[4,4]
  sig.vector <- append(sig.vector, p.value)
}

tmp <- p.cor.data.frame$cols[which(sig.vector < .05)]
asterick.vals <- NULL
for(i in tmp){asterick.vals <- append(asterick.vals,(paste(i, '*', sep='')))}
p.cor.data.frame$cols <- as.character(p.cor.data.frame$cols)
p.cor.data.frame$cols[which(sig.vector < .05)] <- asterick.vals 

mgi.data.bg <- ggplot(p.cor.data.frame, aes(x=cols, y=p.cor.vector, fill=p.cor.vector)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  labs(title = "MGI", x = "QAP Measures", y = "Partial Correllation") + 
  theme(legend.position = 'none')+ 
  scale_y_continuous(limits=c(-.5,.5),oob=rescale_none)

pdf('figure7QapPaper.pdf', height=20, width=20)
#multiplot(go1.data.bg, go2.data.bg, mgi.data.bg, all.data.bg, cols=2)
multiplot(go1.data.bg,mgi.data.bg, cols=2)
dev.off()
