function getDriverDataGDP

clc
clear all
%% -- variables --
timerPeriod = 30;    % seconds


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

years = 1980:2011;


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
        done = false;
        % now loop and check
        
        while ~done
            toc
            pause(timerPeriod)
            fileNm = VarSets.(var).fileName;
            if ~done
                try [f_Handle,done] = GDP.checkProcess;
                catch mssg;
                    f_Handle = [];
                    done = false;
                    disp(mssg)
                    delete mssg
                end
                disp(var);
                if done  % will be first time
                    disp(['***** writing ' YYYY '/' fileNm '.txt *****']);
                    urlwrite(f_Handle,[writeDir YYYY '/' fileNm '.txt']);
                end
            else
                disp(['*----' var ' process complete----*'])
            end

            
            
        end
        clear GDP
    end



end

