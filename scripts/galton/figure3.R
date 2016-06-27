# AFGR June 6 2016
# This script is going to be used to produce a pretty 
# Corellation matrix to be used in the QAP paper
# its going to produce one corellation matrix
# which will include all QAP values across
# all data sets

## Load data
# Start with Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
go1.data <- isolatedVars
go1.data <- go1.data[,order(colnames(go1.data))]
study <- rep('Go1', nrow(go1.data))
go1.data <- cbind(go1.data, study)

# Now do MGI
source('/home/adrose/qapQA/scripts/loadMgiData.R')
detachAllPackages()
mgi.data <- isolatedVars
mgi.data <- mgi.data[,order(colnames(mgi.data))]
study <- rep('MGI', nrow(mgi.data))
mgi.data <- cbind(mgi.data, study)

# Now do Go2
mergedQAP <- mergedQAP.go2
colnames(mergedQAP) <- gsub(pattern='.x', replacement = '', x = colnames(mergedQAP), fixed = TRUE)
isolatedVars <- mergedQAP[qapValNames]
isolatedVars <- cbind(mergedQAP$bblid, isolatedVars)
isolatedVars <- cbind(isolatedVars, mergedQAP[manualQAValue])
colnames(isolatedVars)[1] <- 'bblid'
isolatedVars[manualQAValue] <- as.factor(isolatedVars$averageRating)
size.vars <- grep('size', names(isolatedVars))
isolatedVars <- isolatedVars[, -size.vars]
go2.data <- isolatedVars
go2.data <- go2.data[,order(colnames(go2.data))]
study <- rep('Go2', nrow(go2.data))
go2.data <- cbind(go2.data, study)

# Now combine all of the data
all.cor.data <- rbind(go1.data, mgi.data, go2.data)

## Now load any necasary libraries 
detachAllPackages()
install_load('corrplot')

## Now grab only the desired qap values
valueIndex <- c(4,8,9,13,20,21,27,28,29,30,31,32,35,36)
qapValNames <- qapValNames[valueIndex]

## Now reduce the data to what we want
all.cor.data <- all.cor.data[,qapValNames]
go1.data <- go1.data[,qapValNames]
mgi.data <- mgi.data[,qapValNames]
colnames(all.cor.data) <- c('CNR', 'EFC', 'FBER', 'FWHM',
                            'QI1', 'SNR', 'BG Kurtosis', 
                            'BG Skewness', 'CSF Kurtosis',
                            'CSF Skewness', 'GM Kurtosis',
                            'GM Skewness', 'WM Kurtosis',
                            'WM Skewness') 
qapValNames <- colnames(all.cor.data)
colnames(mgi.data) <- qapValNames
colnames(go1.data) <- qapValNames


# Now produce the correllation matrix
corVals <- cor(all.cor.data[,qapValNames])
corVals.go1 <- cor(go1.data[,qapValNames])
corVals.mgi <- cor(mgi.data[,qapValNames])

# Now produce a pretty plot
pdf('figure3QAPPaper.pdf', height=10, width=20)
par(mfrow=c(1,2))
#corrplot(corVals, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', tl.srt=45, cl.cex=2)
corrplot(corVals.go1, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', tl.srt=45, cl.cex=2, main="PNC")
corrplot(corVals.mgi, 'color', order='alphabet',tl.pos='lt', tl.cex=2, tl.col='black', tl.srt=45, cl.cex=2, main="MGI")
dev.off()
