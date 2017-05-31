function createImages(inputColorTable)

%GLCONSENSUS Consensus community detection using the Louvain-like
% modularity-maximisation procedure, as implemented in genlouvain.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% createImages reads in:
%   (1) the output from T1QA/galton/revisionsR1/computeROIWiseEffectSize-UNIVARIOATE-FS.R
%    It writes out 4 images given the dependent csv image.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load the MNI template surface and annotation files
surf_r='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/surf/rh.white';
surf_l='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/surf/lh.white';

annot_r='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/label/rh.aparc.annot';
annot_l='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/label/lh.aparc.annot';

% Load the input mat file
load(inputColorTable);

%% Start with the right hemisphere data 
[verts,faces] = freesurfer_read_surf(surf_r);
[verts2,labels,colortable] = read_annotation(annot_r);

%% Now loop through each region name 
loopLength=size(vals);
loopLength=loopLength(1);
for i=1:loopLength;
  indexToSearch=vals(i,8);
  hemiIndex=indexToSearch{1}(1:3);
  if hemiIndex == 'rh_'
    % First vreate the string that we are going to grep for
    grepString=indexToSearch{1}(4:end);
    grepString=strrep(grepString, '_thickness','');
    grepString=strrep(grepString, '_volume','');
    % Now find the respective row in the colortable for this roi
    roiValue=strmatch(grepString, colortable.struct_names);
    % now change the color values for this roi
    colortable.table(roiValue, 1) = str2double(vals{i, 2});
    colortable.table(roiValue, 2) = str2double(vals{i, 3});
    colortable.table(roiValue, 3) = str2double(vals{i, 4});
  end
end

%%% Right hemi %%%%

th = trisurf(faces,verts(:,1),verts(:,2),verts(:,3),labels);
set(th,'edgecolor','none');
camlight headlight
axis image;
material shiny
axis off
view(-90,0)
set(gcf,'color','white')
view(0,90)

%%% Left hemi %%%
[verts,faces] = freesurfer_read_surf(surf_l);
[verts2,labels,colortable] = read_annotation(annot_l);
th= trisurf(faces,verts(:,1),verts(:,2),verts(:,3),labels);
set(th,'edgecolor','none');
camlight headlight
axis image;
colormap(jet)
material shiny
axis off
view(-90,0)
set(gcf,'color','white')

h = camlight('left');
for i = 1:20;
   camorbit(10,0)
   camlight(h,'left')
   pause(.1)
end
