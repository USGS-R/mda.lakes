function canopy = getCanopy(WBIC,metaFile)

% gets the average canopy coverage (m) for a given lake
% WBIC is a string;

if eq(nargin,1)
    metaFile = 'supporting files/VegetationHeight_WiLMA.tsv';
end

[indx,values] = wcsFileOpen(metaFile);
useI = strcmp(indx,WBIC);
if any(useI)
    canopy = values(useI);
else
    canopy = NaN;
end

end

