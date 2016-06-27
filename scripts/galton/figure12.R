# AFGR June 13 2016
# This script is going to be used to produce figure ~12?? for the QAP paper
# Figure 12 is going to show the relationship between the composite qap measure,
# rating bins, with cortical thickness


# First thing is first load the data I am only working with Go1 here
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
go1.data <- cbind(mergedQAP$bblid.x, mergedQAP[qapValNames], mergedQAP$averageRating, mergedQAP$sex, mergedQAP$ageAtGo1Scan)
size.vars <- grep('size', names(go1.data))
go1.data <- go1.data[,-size.vars]
colnames(go1.data)[c(1,33,34,35)] <- c('bblid', 'averageRating', 'sex', 'age')


# I now need to resort the data 
go1.data <- go1.data[order(names(go1.data))]

# Now declare the columns of interest 
cols <- names(go1.data)[c(8,9,10,13,14,15,16,19,25,26,29,31,34,35)]

# Now I need to prepare the composite measure 
# To do this I need to load the weights from the Go1 multilogistic model
load('/home/adrose/qapQA/data/tmp6-15/go1Weights.RData')
reg.vals.go <- apply(go1.data[cols], 1, function(x) weighted.mean(x, w))
go1.data <- cbind(go1.data, reg.vals.go)

# I now need to cbind the thickness and mean volume measures from the Go1 data rel
go1.data <- cbind(go1.data, mergedQAP$mprage_fs_mean_thickness)
go1.data <- cbind(go1.data, mergedQAP$mprage_fs_total_area)
go1.data <- cbind(go1.data, go1.data$age^2)
# Now change the column names
colnames(go1.data)[37:39] <- c('meanThickness', 'totalArea', 'ageSquared')

# Now make sure the proper variables are factors
go1.data$sex <- as.factor(go1.data$sex)
go1.data$averageRating <- as.factor(go1.data$averageRating)

# Now load the library(s)
install_load('visreg', 'ggplot2', 'scales','gamm4')

# Now make our linear model 
thicknessModel <- as.formula(paste('meanThickness ~ reg.vals.go * averageRating + age + sex'))
fit <- lm(thicknessModel, data=go1.data)

# Now print the output
pdf("figure12QAPPaper.pdf")
visreg(fit, "reg.vals.go", by="averageRating", overlay=TRUE, xlab="Composite QAP",
       ylab="Mean FS CT Thickness", alpha=1)
visreg(fit, "reg.vals.go")
dev.off()
