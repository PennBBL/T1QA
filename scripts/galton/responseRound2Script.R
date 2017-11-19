# AFGR 

# This script is going to be used to look at the overlap between the flagged freesurfer QA images and the ratings from the images 
# WHat this is going to do is see where the flagged images came from and if any failed our QA procedure.

# Load the data 
source("/home/adrose/T1QA/scripts/galton/loadGo1Data.R")
fsQAVals <- read.csv('~/n1601_t1QaData_20170306.csv')


# Now I need to find the flagged images and where they came from
mergedQAP <- merge(mergedQAP, fsQAVals)

# NOw find a table with the flagged values
flaggedValsTable <- table(mergedQAP$averageManualRating, mergedQAP$fsFlag)
