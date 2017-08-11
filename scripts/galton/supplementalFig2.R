## Load data
# Start with Go1
source('/home/adrose/T1QA/scripts/galton/loadMgiData.R')
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
set.seed(16)

# load library(s)
install_load('caret', 'ggplot2', 'lme4', 'car', 'visreg', 'scales', 'MASS')

# Now lets create our train and validation sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]
validationData <- raw.lme.data[-index,] 
manualQAData$age <- (manualQAData$ageAtGo1Scan / 12)

# Now prep our individual data sets
all.train.data <- merge(trainingData, manualQAData, by='bblid')
all.valid.data <- merge(validationData, manualQAData, by='bblid')

# Now create the values to compare sex bins
bg2.vals.train <- summarySE(data=all.train.data, measurevar='mean_euler', groupvars='sex', na.rm=T)
bg2.vals.train$Dataset <- rep('Training', nrow(bg2.vals.train))
bg2.vals.valid <- summarySE(data=all.valid.data, measurevar='mean_euler', groupvars='sex', na.rm=T)
bg2.vals.valid$Dataset <- rep('Testing', nrow(bg2.vals.valid))
bg2.vals.mgi <- summarySE(data=all.mgi.data, measurevar='mean_euler', groupvars='Gender', na.rm=T)
colnames(bg2.vals.mgi)[1] <- 'sex'
bg2.vals.mgi$Dataset <- 'Validation'
bg2.vals <- rbind(bg2.vals.train, bg2.vals.valid, bg2.vals.mgi)
bg2.vals$Dataset <- factor(bg2.vals$Dataset)
bg2.vals$sex <- c('Male', 'Female', 'Male', 'Female', 'Male', 'Female')

# Now create the plots
bg1 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Training'),], aes(x=factor(sex), y=as.numeric(as.character(mean_euler)), group=Dataset)) +
                geom_bar(stat='identity', position=position_dodge(), width=.5) + 
                labs(title='Training', x='Sex', y='Mean Manual Quality Rating') +
                theme_bw() + 
                coord_cartesian(ylim=c(-110,-250)) +
                       geom_errorbar(aes(ymin=as.numeric(as.character(mean_euler))-se,
                                         ymax=as.numeric(as.character(mean_euler))+se),
                width = .1, position=position_dodge(.9)) +
                #facet_grid(Dataset ~ .) +
                theme(legend.position="none",
                axis.text=element_text(size=20),
                axis.title=element_text(size=30),
                strip.text.y = element_text(size = 16, angle = 270, face="bold"),
                title=element_text(size=30)) + 
		geom_path(aes(x=factor(sex), y=c(-175,-175))) +
		geom_path(aes(x=factor(sex)[1], y=c(-155, -175))) +
		geom_path(aes(x=factor(sex)[2], y=c(-140, -175))) +
		geom_text(aes(x=factor(sex)[1], y=-180), label='*',angle=90, size=10) +
		scale_y_continuous(limits=c(-110, -250),
                           breaks=round(seq(-110, -250, -20), digits=2), oob=rescale_none) +
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.05", hjust=c(1), vjust=c(1.1), size=8, parse=T)

bg2 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Testing'),], aes(x=factor(sex), y=as.numeric(as.character(mean_euler)), group=Dataset)) +
		geom_bar(stat='identity', position=position_dodge(), width=.5) +
		labs(title='Testing', x='Sex', y='Mean Manual Quality Rating') +
		theme_bw() +
		coord_cartesian(ylim=c(-110,-250)) +
		geom_errorbar(aes(ymin=as.numeric(as.character(mean_euler))-se,
		ymax=as.numeric(as.character(mean_euler))+se),
		width = .1, position=position_dodge(.9)) +
		#facet_grid(Dataset ~ .) +
		theme(legend.position="none",
		axis.text.y=element_text(size=20, color='white'),
		axis.text=element_text(size=20),
		axis.title=element_text(size=30),
		axis.ticks.y=element_blank(),
		strip.text.y = element_text(size = 16, angle = 270, face="bold"),
		title=element_text(size=30)) +
		geom_path(aes(x=factor(sex), y=c(-175,-175))) +
		geom_path(aes(x=factor(sex)[1], y=c(-175, -175))) +
		geom_path(aes(x=factor(sex)[2], y=c(-135, -175))) +
		geom_text(aes(x=factor(sex)[1], y=-185), label='***',angle=90, size=10) +
		scale_y_continuous(limits=c(-110, -250),
			breaks=round(seq(-110, -250, -20), digits=2), oob=rescale_none) +
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.001", hjust=c(1), vjust=c(1.1), size=8, parse=T)

pValue <- t.test(all.mgi.data$mean_euler~ all.mgi.data$Gender)
bg3 <- ggplot(bg2.vals[which(bg2.vals$Dataset=='Validation'),], aes(x=factor(sex), y=as.numeric(as.character(mean_euler)), group=Dataset)) +
		geom_bar(stat='identity', position=position_dodge(), width=.5) +
		labs(title='Validation', x='Sex', y='Manual Quality Rating (mean value)') +
		theme_bw() +
		coord_cartesian(ylim=c(-110,-250)) +
		geom_errorbar(aes(ymin=as.numeric(as.character(mean_euler))-se,
		ymax=as.numeric(as.character(mean_euler))+se),
		width = .1, position=position_dodge(.9)) +
		#facet_grid(Dataset ~ .) +
		theme(legend.position="none",
		axis.text.y=element_text(size=20, color='white'),
		axis.title.y=element_text(size=30, color='white'),
		axis.text=element_text(size=20),
		axis.title=element_text(size=30),
		axis.ticks.y=element_blank(),
		strip.text.y = element_text(size = 16, angle = 270, face="bold"),
		title=element_text(size=30)) +
		geom_path(aes(x=factor(sex), y=c(-240,-240))) +
		geom_path(aes(x=factor(sex)[2], y=c(-210, -240))) +
		geom_path(aes(x=factor(sex)[1], y=c(-240, -240))) +
		geom_text(aes(x=factor(sex)[1], y=-245), label='  *',angle=90, size=10) +
		scale_y_continuous(limits=c(-110, -250),
			breaks=round(seq(-110, -250, -20), digits=2), oob=rescale_none) +
		annotate("text", x=c(Inf), y=c(Inf), label="p < 0.05", hjust=c(1), vjust=c(1.1), size=8, parse=T)

# Now plot the age relationships
corVal <- cor(all.train.data$mean_euler, all.train.data$age, method='spearman', use='complete')
corSig <- cor.test(all.train.data$mean_euler, all.train.data$age, method='spearman')$p.value
corText1 <- expression(~rho == .44)
corText2 <- paste("p < 0.0001")
mod1 <- ggplot(all.train.data, aes(y=mean_euler, x=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(xlim=c(8,22), ylim=c(-250,-80)) +
   labs(title='', y='Mean Manual Quality Rating', x='Age (years)') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30)) +
#scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


corVal <- cor(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')
corSig <- cor.test(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')$p.value
corText1 <- expression(~rho == paste(0.31))
corText2 <- paste("p < 0.01")
mod2 <- ggplot(all.valid.data, aes(y=mean_euler, x=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(xlim=c(8,22), ylim=c(-250,-80)) +
   labs(title='', y='Mean Manual Quality Rating', x='Age (years)') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30),
    axis.title.y=element_text(size=20, color='white'),
    axis.text.y=element_text(size=30, color='white'),
    axis.ticks.y=element_blank()) +
#    scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


corVal <- cor(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')
corSig <- cor.test(all.valid.data$rawAverageRating.x, all.valid.data$age, method='spearman')$p.value
corText1 <- expression(~rho == paste(0.31))
corText2 <- paste("p < 0.01")
mod2 <- ggplot(all.valid.data, aes(y=mean_euler, x=age)) +
   geom_smooth(method=lm, color='black') +
   theme_bw() +
   coord_cartesian(xlim=c(8,22), ylim=c(-250,-80)) +
   labs(title='', y='Mean Manual Quality Rating', x='Age (years)') +
   theme(
    axis.text=element_text(size=20),
    axis.title=element_text(size=30),
    axis.title.y=element_text(size=20, color='white'),
    axis.text.y=element_text(size=30, color='white'),
    axis.ticks.y=element_blank()) +
#    scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
   annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)



corVal <- cor(all.mgi.data$mean_euler, all.mgi.data$age, method='spearman', use='complete')
corSig <- cor.test(all.mgi.data$rawAverageRating, all.mgi.data$age, method='spearman')$p.value
corText1 <-expression(~rho == paste(0.06))
corText2 <- paste("p < 0.1")
mod3 <- ggplot(all.mgi.data, aes(y=mean_euler, x=age)) +
geom_smooth(method=lm, color='black') +
theme_bw() +
coord_cartesian(xlim=c(20,70), ylim=c(-250,-80)) +
labs(title='', y='Manual Quality Rating (mean value)', x='Age (years)') +
theme(
axis.text=element_text(size=20),
axis.title=element_text(size=30),
axis.title.y=element_text(size=20, color='white'),
axis.text.y=element_text(size=30, color='white'),
axis.ticks.y=element_blank()) +
#scale_y_continuous(breaks=c(0,.33,.66,1,1.33,1.66,2)) +
#scale_x_continuous(breaks=c(20,40,60,80)) +
annotate("text", x=c(Inf, Inf), y=c(-Inf, -Inf), label=c(as.character(corText2), as.character(corText1)), hjust=c(1, 1), vjust=c(-.5, -2.5), size=8, parse=T)


png('supfigure3-demographicsvsEulerPaper.png', width=24, height=16, units='in', res=300)
multiplot(bg1, mod1, bg2, mod2, bg3, mod3, cols=3)
dev.off()
