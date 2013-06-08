function elev = getElev(WBIC,metaFile)

% gets the elevation (m) for a given lake
% WBIC is a string;

if eq(nargin,1)
    metaFile = 'supporting files/WI_ManagedLakes_elevation.tsv';
end

[indx,values] = wcsFileOpen(metaFile);
useI = strcmp(indx,WBIC);
if any(useI)
    elev = values(useI);
else
    elev = NaN;
end

end

