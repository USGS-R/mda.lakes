function getDriverDataGDP

clc
clear all
%% -- variables --
timerPeriod = 22;    % seconds
retries = 1;


writeDir= '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';
GeoServ = 'https://www.sciencebase.gov/catalogMaps/mapping/ows/50d35261e4b062c7914ebd14';

SW_URI  = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
LW_URI  = 'dods://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR.dailyavgs/monolevel/dlwrf.YYYY.nc';
air_URI = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
uwnd_URI = 'dods://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR.dailyavgs/monolevel/uwnd.10m.YYYY.nc';
vwnd_URI = 'dods://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR.dailyavgs/monolevel/vwnd.10m.YYYY.nc';
prcp_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
vprP_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
snow_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';

srad = struct('uri',SW_URI,'fileName','SW_daymet');
dlwrf= struct('uri',LW_URI,'fileName','LW_NARR');
tmax = struct('uri',air_URI,'fileName','airMx_daymet');
tmin = struct('uri',air_URI,'fileName','airMn_daymet');
uwnd = struct('uri',uwnd_URI,'fileName','uwnd_NARR');
vwnd = struct('uri',vwnd_URI,'fileName','vwnd_NARR');
prcp = struct('uri',prcp_URI,'fileName','prcp_daymet');
vp   = struct('uri',vprP_URI,'fileName','VP_daymet');
swe  = struct('uri',snow_URI,'fileName','snow_daymet');


VarSets = struct('srad',srad,'dlwrf',dlwrf,'tmax',tmax,'tmin',tmin,...
    'uwnd',uwnd,'vwnd',vwnd,'prcp',prcp,'vp',vp,'swe',swe);

feature_collection = 'sb:managedLakesAllOne';
attribute = 'OBJECTID';

years = 2007:2011;

%% set processing

vars = fieldnames(VarSets);

for i = 1:length(years)
    YYYY = num2str(years(i));
    % create folder if it doesn't exist
    mkdir(writeDir,YYYY)
    for v = 1:length(vars)
        tic
        GDP = mGDP();
        
        GDP = GDP.setGeoserver(GeoServ);
        GDP = GDP.setFeature('FEATURE_COLLECTION',feature_collection,...
            'ATTRIBUTE',attribute,'GML','NULL');
        
        
        URI = regexprep(VarSets.(vars{v}).uri,'YYYY',YYYY);
        disp(URI)
        var = vars{v};
        GDP = GDP.setPostInputs('DATASET_ID',var,...
            'TIME_START',[YYYY '-01-01T00:00:00.000Z'],...
            'TIME_END',  [YYYY '-12-31T00:00:00.000Z']);
        GDP = GDP.setDatasetURI(URI);
        GDP = GDP.executePost;
        attempt = 1;
        retry = false;
        done = false;
        
        % now loop and check 
        while ~done
            toc
            pause(timerPeriod)
            if retry && ~gt(attempt,retries)
                GDP = GDP.executePost;
                attempt = attempt+1;
                retry = false;
            elseif retry
                done = true;
                disp(['Process failed after ' num2str(attempt)...
                    ' attempts']);
            end
            fileNm = VarSets.(var).fileName;
            if ~done    % if still not done
                disp(['currently working on ' var]);
                
                % status can be failed, complete, incomplete, none, or unknown
                [f_Handle,status] = GDP.checkProcess;   % check again
                if strcmp(status,'failed')
                    retry = true;       % need to try again with execute
                elseif strcmp(status,'complete')
                    done = true;
                elseif strcmp(status,'none')
                    error('no process started')
                end
                
                
                if done  % will be first time that done is true
                    disp(['***** writing ' YYYY '/' fileNm '.txt *****']);
                    urlwrite(f_Handle,[writeDir YYYY '/' fileNm '.txt']);cd ..
                end
            elseif done && retry
                disp(['#$(&@#& '  var ' process failed #$&(@#$'])
            else
                disp(['*----' var ' process complete----*'])
            end    
            disp(status)
        end
        clear GDP     
    end
end

