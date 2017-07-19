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
surf_r='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/surf/rh.inflated';
surf_l='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/surf/lh.inflated';

annot_r='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/label/rh.aparc.annot';
annot_l='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/MNI_1mm_template/label/lh.aparc.annot';

% Load the input mat file
load(inputColorTable);

%% Start with the right hemisphere data 
[verts,faces] = freesurfer_read_surf(surf_r);
[verts2,labels,colortable] = read_annotation(annot_r);
label_col = zeros(size(labels));
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
    % Now find the original label value
    origLabelValue=colortable.table(roiValue,5);
    % Now find the new value that should be assigned to this value
    % we are going to scale the data between 0 and 1
    % and this range value will be the new label value
    % careful steps will have to be made to ensure that all
    % non significant ROI's are greyed out 
    newValues=(str2double(vals(:,9))-min(str2double(vals(:,9))))/range(str2double(vals(:,9)));
    newValue=str2double(vals(i,9));
    % now change all of the corresponding label values to our new value 
    indx = labels == origLabelValue;
    label_col(indx) = newValue;
  end
end

% Create a custom color map 
% This will be a little bit tricky as I need to accomplish a couple of things here:
% 1.) Gray out the non signifianct relationships 
% 2.) Ensure that the colors indicate the correct directions of relationships:
%    a. Hot must be larger 
%    b. blue must be smaller 
tmp_cmp=jet(loopLength);
% Now find which rows have been greyed out in the input 
indx=find(ismember(str2double(vals(:,2:4)), [190 190 190], 'rows'));
% Now check to see if we have any gray values
valueToCheck=size(indx);
valueToCheck=valueToCheck(1);
if (valueToCheck > 0)
  % Now grey out the corresponding values
  tmp_cmp(min(indx):max(indx),:)=repmat(0.7, range(indx)+1, 3);
end
% Now remove the green from the color map
newMapLength=loopLength-max(indx);
subtmpcolmap=autumn(newMapLength);
tmp_cmp(max(indx)+1:end,:)=subtmpcolmap;
% Now check to see if we did not have any gray images 
% If we didn't export jet as the color table 
if (valueToCheck == 0)
  % Find the positive values and create a red-yellow heat map for those
  indx=find(str2double(vals(:,9))<0);
  tmp_cmp(0:max(indx),:)=cool(max(indx));
  indx2=find(str2double(vals(:,9))>0);
  tmp_cmp(max(tmp_cmp):max(indx2),:)=hot(max(indx2))
end

% Now plot the surfaces
hFig = trisurf(faces,verts(:,1),verts(:,2),verts(:,3),label_col);
set(hFig,'edgecolor','none');
axis image;
material dull
axis off
colormap(flipud(tmp_cmp));
view(-90,0)
camlight headlight
saveas(hFig, 'rhLateral.png')
export_fig rhLateral.png -transparent

hFig=figure();
hFig = trisurf(faces,verts(:,1),verts(:,2),verts(:,3),label_col);
set(hFig,'edgecolor','none');
axis image;
material dull
axis off
colormap(flipud(tmp_cmp));
view(-280,0)
camlight headlight
saveas(hFig, 'rhMedial.png')
export_fig rhMedial.png -transparent

% Now replicate this whole procedure for the left hemisphere 
% There is a better more line efficent manner to perform this but I am not pursuing that atm.
%% Start with the right hemisphere data 
[verts,faces] = freesurfer_read_surf(surf_l);
[verts2,labels,colortable] = read_annotation(annot_l);
label_col = zeros(size(labels));
%% Now loop through each region name 
loopLength=size(vals);
loopLength=loopLength(1);
for i=1:loopLength;
  indexToSearch=vals(i,8);
  hemiIndex=indexToSearch{1}(1:3);
  if hemiIndex == 'lh_'
    % First vreate the string that we are going to grep for
    grepString=indexToSearch{1}(4:end);
    grepString=strrep(grepString, '_thickness','');
    grepString=strrep(grepString, '_volume','');
    % Now find the respective row in the colortable for this roi
    roiValue=strmatch(grepString, colortable.struct_names);
    % Now find the original label value
    origLabelValue=colortable.table(roiValue,5);
    % Now find the new value that should be assigned to this value
    % we are going to scale the data between 0 and 1
    % and this range value will be the new label value
    % careful steps will have to be made to ensure that all
    % non significant ROI's are greyed out 
    newValues=(str2double(vals(:,9))-min(str2double(vals(:,9))))/range(str2double(vals(:,9)));
    newValue=str2double(vals(i,9));
    % now change all of the corresponding label values to our new value 
    indx = labels == origLabelValue;
    label_col(indx) = newValue;
  end
end

% Now plot the surfaces
hFig = trisurf(faces,verts(:,1),verts(:,2),verts(:,3),label_col);
set(hFig,'edgecolor','none');
axis image;
material dull
axis off
colormap(flipud(tmp_cmp));
view(-90,0)
camlight headlight
saveas(hFig, 'lhLateral.png')
export_fig lhLateral.png -transparent

hFig=figure();
hFig = trisurf(faces,verts(:,1),verts(:,2),verts(:,3),label_col);
set(hFig,'edgecolor','none');
axis image;
material dull
axis off
colormap(flipud(tmp_cmp));
view(-280,0)
camlight headlight
saveas(hFig, 'lhMedial.png')
export_fig lhMedial.png -transparent
