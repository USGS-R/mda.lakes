function zMax = getZmax(WBIC,metaFile)

% gets the max depth value for a given lake
% WBIC is a string;

ft2m = 0.3048;
if eq(nargin,1)
    metaFile = ['/Users/jread/Desktop/Science Projects/'...
        'WiLMA/GLM files/Metadata/managed_lake_info.txt'];
end
wbic_i = 1;
maxZ_i = 6;
reader = '%s %s %s %s %s %s %s %s %s %s %s %s';

fID = fopen(metaFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};
depths = dat{maxZ_i};

useI = strcmp(WBICs,WBIC);
if any(useI)
    depths = str2double(depths(useI));
    depths = depths(~isnan(depths));
    zMax = mean(depths)*ft2m;
else
    zMax = NaN;
end




end

