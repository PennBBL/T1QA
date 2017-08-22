val='/home/arosen/fsctColorScale.mat'
produceFSImagesMediation(val)
system('mv *png /home/arosen/forT1QA/train/')
val='/home/arosen/fsctColorScaleTest.mat'
produceFSImagesMediation(val)
system('mv *png /home/arosen/forT1QA/test/')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctColorScale.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsct')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctvalidColorScale.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsctvalid')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsvolColorScale.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsvol')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsvolvalidColorScale.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsvolvalid')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctColorScaleMGI.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsctmgi')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsvolColorScaleMGI.mat'
produceFSImages(val)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsvolmgi')
