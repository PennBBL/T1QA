val='/home/arosen/T1QA/data/fsctColorScale.mat'
produceFSImages(val)
system('mv *png /home/arosen/T1QA/data/fsct')
val='/home/arosen/T1QA/data/fsctvalidColorScale.mat'
produceFSImages(val)
system('mv *png /home/arosen/T1QA/data/fsctvalid')
val='/home/arosen/T1QA/data/fsvolColorScale.mat'
produceFSImages(val)
system('mv *png /home/arosen/T1QA/data/fsvol')
val='/home/arosen/T1QA/data/fsvolvalidColorScale.mat'
produceFSImages(val)
system('mv *png /home/arosen/T1QA/data/fsvolvalid')