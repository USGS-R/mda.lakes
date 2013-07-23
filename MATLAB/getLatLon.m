function [lat, lon] = getLatLon(WBIC,metaFile)
% gets the Lat/Lon for a given lake
% WBIC is a string;

persistent dStore;

if (nargin == 1)
    metaFile = 'supporting files/WI_Lakes_WbicLatLon.tsv';
end

if(isempty(dStore))
    dStore = buildDStore(metaFile);
end

if(isnumeric(WBIC))
    WBIC = num2str(WBIC);
end

if dStore.isKey(WBIC)
    latLon = dStore(WBIC);
    latLon = latLon{1};
    lat = latLon(1);
    lon = latLon(2);
else
    lat = NaN;
    lon = NaN;
end

end

%This is somewhat slow, but it only needs to be run once. Then
% this overall function is really fast.
function d = buildDStore(metaFile)
    
    rawD = importdata(metaFile);
    
    d = containers.Map;
    
    for i = 1:size(rawD.data,1)
        key = num2str(rawD.data(i,1));  %Convert to string because we're using string WBICs
        d(key) = {rawD.data(i,2:3)};
    end
    
end

