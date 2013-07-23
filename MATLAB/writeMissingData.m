function writeMissingData()



% open reference file for WBICs
refFile = 'all_bass_wae_wbic.txt';

rawDir = '/Users/jread/Desktop/Science Projects/WiLMA/GLM files/Metadata/';

metaFile = '/Users/jread/Desktop/Science Projects/WiLMA/GLM files/Metadata/managed_lake_info.txt';

fID = fopen([rawDir refFile]);
format = '%s';

dat = textscan(fID,format,'Delimiter','\t','TreatAsEmpty','NA','HeaderLines',1);

missingWBICs = dat{1};
fclose all;


dataFormat = '%s,%s,%s,%s\n'; %WBIC,area?,zMax?,bathy?

fileName = [rawDir 'WallyBassyINFO.csv'];
fid = fopen(fileName,'w');
headers = 'WBIC,area?,zMax?,bathy?\n';
fprintf(fid,headers);

fclose all;

lakeIDs = getLakeIDs();


for lk = 1:length(missingWBICs)
    % check for bathy info, 
    WBIC = missingWBICs{lk};
    % check for exists on managed lakes 
    info = {WBIC,'FALSE','FALSE','FALSE'};
    if ~any(strcmp(WBIC,lakeIDs))
        % missing....
    else [bth,fromBth] = getBathy(WBIC);
        if fromBth
            info{4} = 'TRUE';
        end
        zMax = getZmax(WBIC,metaFile);
        if ~isnan(zMax)
            info{3} = 'TRUE';
        end
        
        lkeA = getArea(WBIC,metaFile);
        if ~isnan(lkeA)
            info{2} = 'TRUE';
        end
        
    end
    fid = fopen(fileName,'a');
    fprintf(fid,[info{1} ',' info{2} ',' info{3} ',' info{4} '\n']);
    fclose all;
end


fclose all;

end

