function getDriverDataGDP

clear all
%% -- variables --
timerPeriod = 4;    % seconds


writeDir= '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';
GeoServ = 'https://www.sciencebase.gov/catalogMaps/mapping/ows/50d35261e4b062c7914ebd14';

SW_URI  = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
LW_URI  = 'dods://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR.dailyavgs/monolevel/dlwft.YYYY.nc';
air_URI = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
wnd_URI = 'dods://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR.dailyavgs/monolevel/uwnd.10m.YYYY.nc';
prcp_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
vprP_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';
snow_URI= 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet';

srad = struct('uri',SW_URI,'fileName','SW_daymet');
dlwft= struct('uri',LW_URI,'fileName','LW_NARR');
tmax = struct('uri',air_URI,'fileName','airMx_daymet');
tmin = struct('uri',air_URI,'fileName','airMn_daymet');
uwnd_10m = struct('uri',wnd_URI,'fileName','wnd_NARR');
Prcp = struct('uri',prcp_URI,'fileName','prcp_daymet');
vp   = struct('uri',vprP_URI,'fileName','VP_daymet');
swe  = struct('uri',snow_URI,'fileName','snow_daymet');


VarSets = struct('srad',srad,'dlwft',dlwft,'tmax',tmax,'tmin',tmin,...
    'uwnd_10m',uwnd_10m,'Prcp',Prcp,'vp',vp,'swe',swe);

feature_collection = 'sb:managedLakesAllOne';
attribute = 'OBJECTID';

years = 1980:2011;


%% set processing

vars = fieldnames(VarSets);

for i = 1:length(years)
    YYYY = num2str(years(i));
    % create folder if it doesn't exist
    clear GDP
    mkdir(writeDir,YYYY)
    done = false(length(vars),1);
    for v = 1:length(vars)
        GDP(v) = mGDP();
        
        GDP(v) = GDP(v).setGeoserver(GeoServ);
        GDP(v) = GDP(v).setFeature('FEATURE_COLLECTION',feature_collection,...
            'ATTRIBUTE',attribute,'GML','NULL');
        
        
        URI = regexprep(VarSets.(vars{v}).uri,'YYYY',YYYY);
        
        var = regexprep(vars{v},'_','.');
        GDP(v) = GDP(v).setPostInputs('DATASET_ID',var,...
            'TIME_START',[YYYY '-01-01T00:00:00.000Z'],...
            'TIME_END',  [YYYY '-01-10T00:00:00.000Z']);
        GDP(v) = GDP(v).setDatasetURI(URI);
        GDP(v) = GDP(v).executePost;
        done(v) = false;
    end
    % now loop and check
    tic
    while any(~done)   
        toc
        pause(timerPeriod)
        for v = 1:length(vars)
            var = vars{v};
            fileNm = VarSets.(var).fileName;
            if ~done(v)
                [f_Handle,done(v)] = GDP(v).checkProcess;
                disp(var);
                if done(v)  % will be first time
                    urlwrite(f_Handle,[writeDir YYYY '/' fileNm '.txt']);
                end
            end
        end
    end

    
end

    



end

