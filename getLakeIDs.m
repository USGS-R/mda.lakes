function lakeIDs = getLakeIDs()



metaFile = ['/Users/jread/Desktop/Science Projects/'...
    'WiLMA/GLM files/Metadata/managed_lake_info.txt'];
wbic_i = 1;
reader = '%s %s %s %s %s %s %s %s %s %s %s %s';

fID = fopen(metaFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};

lakeIDs = unique(WBICs);



end

