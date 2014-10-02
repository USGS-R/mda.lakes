function refreshWiLMAfiles

bs = filesep;

finalDir = ['supporting files' bs];
tempDir = [finalDir 'tmp' bs];
if ~exist([finalDir 'tmp' bs],'dir')
    mkdir(tempDir(1:end-1));
end

SBu = 'https://www.sciencebase.gov/catalog/file/get/';

%% metadata file:
fName.info = 'managed_lake_info.txt';
SB.info = '51afa117e4b08a3322c53ff3';

%% Vegetation heights:
fName.veg = 'VegetationHeight_WiLMA.tsv';
SB.veg = '51afbc2de4b08a3322c5bbbd';

%% lake elevations:
fName.elv = 'WI_ManagedLakes_elevation.tsv';
SB.elv = '51afc803e4b08a3322c5f069';

%% GLM .nml file:
fName.nml = 'glmDEFAULT.nml';
SB.nml = '51b27656e4b022a6a540fa8a';

%% secchi observations:
fName.secc = 'annual_mean_secchi.txt';
SB.secc = '51b2777ee4b022a6a540fa8f';

fN = fieldnames(SB);
for j = 1:length(fN)
    disp(['..pulling down ' SB.(fN{j}) ' to replace current ' fName.(fN{j})])
    nme = unzip([SBu SB.(fN{j})],tempDir); % file handle
    copyfile(nme{1},[finalDir fName.(fN{j})]);
end

% bathy files
SB.bth = '51af6fd4e4b08a3322c44897';
disp(['..pulling down ' SB.bth ' to replace current bathy files'])
nme = unzip([SBu SB.bth],tempDir); % file handle
for j = 1:length(nme)
    name = regexp(nme{j},bs,'split');
    copyfile(nme{j},[finalDir 'Bathy' bs name{end}]);
end

rmdir(tempDir(1:end-1),'s')
end

