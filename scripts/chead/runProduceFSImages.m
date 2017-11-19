val='/home/arosen/fsctColorScale.mat'
produceFSImages(val)
system('mv *png /home/arosen/forT1QA/train/')
val='/home/arosen/fsctColorScaleTest.mat'
produceFSImages(val)
system('mv *png /home/arosen/forT1QA/test/')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctColorScale.mat'
outMap = produceFSImages(val);
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsct')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctvalidColorScale.mat'
produceFSImages(val, outMap)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsctvalid')
val='/data/joy/BBL/projects/pncT1QA/T1QA/data/fsctColorScaleMGI.mat'
produceFSImages(val)%, outMap)
system('mv *png /data/joy/BBL/projects/pncT1QA/T1QA/data/fsctmgi')

