function zMean = getZmean(WBIC,metaFile)

% gets the max depth value for a given lake
% WBIC is a string;

ft2m = 0.3048;
if eq(nargin,1)
    metaFile = '../supporting files/managed_lake_info.txt';
end
wbic_i = 1;
meanZ_i = 7;
reader = '%s %s %s %s %s %s %s %s %s %s %s %s';

fID = fopen(metaFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};
depths = dat{meanZ_i};
zMean = NaN;
useI = strcmp(WBICs,WBIC);
if any(useI)
    depths = str2double(depths(useI));
    depths = depths(~isnan(depths));
    zMean = mean(depths)*ft2m;
end
if isnan(zMean)
    zMean = 1/3*getZmax(WBIC,metaFile);
end




end

