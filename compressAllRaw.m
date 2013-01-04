function compressAllRaw
% loops through directory and calls zipLakeFile for each lake in the
% directory with the associated extension

%% -- variables --
ext = '.prj';    % extension for sorting and finding file names
rootDir = '/Volumes/projects/WiLMA/rawLakes/';
appendNm = 'WiLMA_lake_';
zipDir  = '/Volumes/projects/WiLMA/zippedLakes/';


%% begin function
availFiles = dir(fullfile([rootDir '*' ext]));

for i = 1:length(availFiles)
    fileNm = availFiles(i).name; % will be char
    lakeNm = fileNm(1:end-length(ext));
    % * run zip function 
    zipLakeFile(lakeNm,appendNm,rootDir,zipDir)

end

