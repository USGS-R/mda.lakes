function [bth,fromBth] = getBathy(WBIC)

% get or estimate bathemetry 
% bth is (2,n); (1,n) is bthH, (2,n) is bthA
% assumes m2 area!!!
numZ= 15;

rootDir = ['/Users/jread/Desktop/Science Projects/'...
    'WiLMA/GLM files/Metadata/'];

%% first, look for bathy file
fromBth = false;
fID = fopen([rootDir '/Bathy/' WBIC '.bth']);
if gt(fID,0)
    % parse..
    dat = textscan(fID,'%f %f','Delimiter','\t','HeaderLines',1);
    bthH = dat{1}';
    bthA = dat{2}';
    fromBth = true;
else
    % look for max depth and area
    zMax = getZmax(WBIC);
    lkeA = getArea(WBIC);
    if ~any(isnan([zMax lkeA]))
        bthH = linspace(0,zMax,numZ);
        bthA = interp1([0 zMax],[lkeA 0],bthH);
    else
        bthA = NaN;
        bthH = NaN;
    end
end
bth = [bthH; bthA];

fclose all;


