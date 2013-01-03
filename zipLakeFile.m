function zipLakeFile(lakeNm)
% aggregates names, appends a name to the begining of the zip file,
% re-writes the .prj file for each lake (to conform to prj2epsg web
% services)

% - lakeNm must be a string

if ~isstring(lakeNm)
    error('lake name input must be a string')
end

%% -- variables --
appendNm = 'WiLMA_';
rootDir = '/Volumes/projects/WiLMA/';
zipDir  = '/Volumes/projects/WiLMA/zipp[edLakes/';
projReWrite = ['GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",'...
    'SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM'...
    '["Greenwich",0],UNIT["Degree",0.017453292519943295]]'];

%% begin process
useFiles = dir(fullfile(matlabroot, ...
               'toolbox/matlab/audiovideo/*.m'))



end

