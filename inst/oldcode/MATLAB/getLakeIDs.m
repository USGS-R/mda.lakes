function lakeIDs = getLakeIDs(metaFile)


if eq(nargin,0)
    metaFile = '../supporting files/managed_lake_info.txt';
end
wbic_i = 1;
reader = '%s %s %s %s %s %s %s %s %s %s %s %s';

fID = fopen(metaFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};

lakeIDs = unique(WBICs);



end

