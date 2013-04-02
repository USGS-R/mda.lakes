function lkeArea = getArea(WBIC)

% gets the max depth value for a given lake
% WBIC is a string;

acre2m2 = 4046.85642;

metaFile = ['/Users/jread/Desktop/Science Projects/'...
    'WiLMA/GLM files/Metadata/managed_lake_info.txt'];
wbic_i = 1;
lkeA_i = 4;
reader = '%s %s %s %s %s %s %s %s %s %s %s %s';

fID = fopen(metaFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};
areas = dat{lkeA_i};

useI = strcmp(WBICs,WBIC);
if any(useI)
    areas = str2double(areas(useI));
    areas = areas(~isnan(areas));
    lkeArea = mean(areas)*acre2m2;
else
    lkeArea = NaN;
end




end

