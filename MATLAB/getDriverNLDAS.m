function GDP = getDriverNLDAS

clc
%% -- variables --
timerPeriod = 10;   % seconds between checking
retries = 5;        % times to try again after failed process
writeDir= './Driver data/';


% - new geoserver location defined - 
GeoServ = 'https://www.sciencebase.gov/catalogMaps/mapping/ows/50d35261e4b062c7914ebd14';
WPS     =   'http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/gdp-process-wps/WebProcessingService';

% - file ouput specifications - 

% - feature information related to the shapefile - 
feature_collection = 'sb:managedLakesAllOne';
attribute = 'WBDY_WBIC'; % for WBIC codes... 

% - URI location and variable name - 

URI = 'dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best';

URI = 'dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas_!2/best';
vars = {'Downward_shortwave_radiation_flux_surface',...
    'Downward_longwave_radiation_flux_surface',...
    'Temperature_height_above_ground',...
    'u-component_of_wind_height_above_ground',...
    'v-component_of_wind_height_above_ground',...
    'Pressure_surface',...
    'Specific_humidity_height_above_ground',...
    'Total_precipitation_surface_1_Hour_Accumulation'}
disp(URI) % display the processing target dataset

    fileN = ['NLDAS_2011-2012'];
    varN = fileN
    
    %% set processing
    GDP = mGDP();       % instantiate the mGDP object
    
    % - set fields or modify fields from the mGDP defaults -
    GDP = GDP.setGeoserver(GeoServ);
    
    
    GDP = GDP.setWPS(WPS);
    GDP = GDP.setFeature('FEATURE_COLLECTION',feature_collection,...
        'ATTRIBUTE',attribute,'GML','NULL');
    GDP = GDP.setPostInputs('DATASET_ID',vars,...
        'TIME_START','2011-01-01T00:00:00.000Z',...
        'TIME_END',  '2012-12-31T23:00:00.000Z');
    GDP = GDP.setDatasetURI(URI);
    
    % - excecute the defined mGDP post -
    GDP = GDP.executePost;
