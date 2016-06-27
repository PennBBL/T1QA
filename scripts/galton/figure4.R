# AFGR June 6 2016
# This script is going to be used to plot the mean and SEM for the qap values of choice
# It is going to produce a 4x4 matrix with all of the values of interest
# I am only going to make this for 
#	1.) efc
#	2.) fber
#	3.) snr
#	4.) wm.skewness
#	5.) bg.*

## Load data
# Start with Go1
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
go1.data <- isolatedVars
go1.data <- go1.data[,order(colnames(go1.data))]
study <- rep('PNC 1', nrow(go1.data))
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
study <- rep('PNC 2', nrow(go2.data))
go2.data <- cbind(go2.data, study)

# Now combine all of the data
#all.cor.data <- rbind(go1.data, mgi.data, go2.data)
all.cor.data <- rbind(go1.data, mgi.data)
all.cor.data$averageRating <- round(as.numeric(as.character(all.cor.data$averageRating)), digits=2)
all.cor.data$averageRating[all.cor.data$averageRating==1.33] <- 1
## Now load the proper libraries 
install_load('ggplot2')

# Now summarySE all qap Values of interest which include:
#	1.) efc
#	2.) fber
#	3.) snr
#	4.) wm.skewness
#	5.) bg.*

se.efc <- summarySE(all.cor.data, measurevar='efc', groupvars=c('averageRating', 'study'))
se.fber <- summarySE(all.cor.data, measurevar='fber', groupvars=c('averageRating', 'study'))
se.snr <- summarySE(all.cor.data, measurevar='snr', groupvars=c('averageRating', 'study'))
se.wm.skewness <- summarySE(all.cor.data, measurevar='wm.skewness', groupvars=c('averageRating', 'study'))
se.bg.kurtosis <- summarySE(all.cor.data, measurevar='bg.kurtosis', groupvars=c('averageRating', 'study'))
se.bg.skewness <- summarySE(all.cor.data, measurevar='bg.skewness', groupvars=c('averageRating', 'study'))

efc.bg <- ggplot(se.efc, aes(x=factor(averageRating), y=efc, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=efc-se, ymax=efc+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average EFC Value', x='Average Quality Rating', y='EFC')

fber.bg <- ggplot(se.fber, aes(x=factor(averageRating), y=fber, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=fber-se, ymax=fber+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average FBER Value', x='Average Quality Rating', y='FBER') 

snr.bg <- ggplot(se.snr, aes(x=factor(averageRating), y=snr, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=snr-se, ymax=snr+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average SNR Value', x='Average Quality Rating', y='SNR') 

wm.bg <- ggplot(se.wm.skewness, aes(x=factor(averageRating), y=wm.skewness, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=wm.skewness-se, ymax=wm.skewness+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average WM Skewness Value', x='Average Quality Rating', y='WM Skewness') 

bg.bg.1 <- ggplot(se.bg.kurtosis, aes(x=factor(averageRating), y=bg.kurtosis, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=bg.kurtosis-se, ymax=bg.kurtosis+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average BG Kurtosis Value', x='Average Quality Rating', y='BG Kurtosis') 

bg.bg.2 <- ggplot(se.bg.skewness, aes(x=factor(averageRating), y=bg.skewness, fill=factor(averageRating))) + 
                       geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                       geom_errorbar(aes(ymin=bg.skewness-se, ymax=bg.skewness+se), 
                       width = .2, position=position_dodge(.9)) +
                       facet_grid(. ~ study, scales="free", space="free_y") +
                       theme_bw() +
                       theme(legend.position="none") +
                       labs(title='Average BG Skewness Value', x='Average Quality Rating', y='BG Skewness') 
pdf('figure4QAPPaper.pdf', height=16, width=16)
multiplot(efc.bg, fber.bg, snr.bg, bg.bg.1, cols=2)
dev.off()
