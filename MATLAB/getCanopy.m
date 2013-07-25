function canopy = getCanopy(WBIC,metaFile)

% gets the average canopy coverage (m) for a given lake
% WBIC is a string;

if eq(nargin,1)
    metaFile = '../supporting files/VegetationHeight_WiLMA.tsv';
    [indx,values] = wcsFileOpen(metaFile);
    useI = strcmp(indx,WBIC);
    if any(useI)
        canopy = values(useI);
    else
        canopy = NaN;
    end

else
    
    fID = fopen(metaFile);
    dat = textscan(fID,'%s %f','Delimiter',',','HeaderLines',1);
    fclose(fID);
    indx = dat{1};
    values = dat{2};
    useI = strcmp(indx,WBIC);
    if any(useI)
        canopy = values(useI);
    else
        canopy = NaN;
    end


end

