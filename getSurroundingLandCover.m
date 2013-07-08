function getSurroundingLandCover

clear all
%% -- variables --
timerPeriod = 30;    % seconds


writeDir= '/Users/jread/Desktop/Science Projects/WiLMA/NLCD data/';
GeoServ = 'https://www.sciencebase.gov/catalogMaps/mapping/ows/50e72cf8e4b00c3282549a83';

URI  = 'http://raster.nationalmap.gov/ArcGIS/services/TNM_LandCover/MapServer/WCSServer';
dataID= 'Land_Cover_2006_1';

feature_collection = 'sb:managedLakes_donut';
attribute = 'OBJECTID';


%% set processing

GDP = mGDP();

GDP = GDP.setAlgorithm('FCGC');
GDP = GDP.setGeoserver(GeoServ);
GDP = GDP.setFeature('FEATURE_COLLECTION',feature_collection,...
    'ATTRIBUTE',attribute,'GML','NULL');

GDP = GDP.setPostInputs('DATASET_ID',dataID);
GDP = GDP.setDatasetURI(URI);
GDP = GDP.executePost;
done = false;
% now loop and check
tic
while ~done
    toc
    pause(timerPeriod)
    fileNm = 'NCLD_donuts';
    if ~done
        try [f_Handle,done] = GDP.checkProcess;
        catch mssg;
            f_Handle = [];
            done = false;
            delete(mssg)
        end
        if done  % will be first time
            disp(['***** writing ' fileNm '.txt *****']);
            urlwrite(f_Handle,[writeDir fileNm '.txt']);
        end
    else
        disp('*----process complete----*')
    end
    
end
clear GDP