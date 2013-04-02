function writeGLMdriver()


lakeName  = 'Sparkling';
lakeID = struct('Sparkling','1881900','Mendota','805400');
acre2m2 = 4046.85642;
daily = false;

rawDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';
fileN = 'GLM_GDP_driver';
if daily 
    rawDir = [rawDir 'Dailies/'];
    fileN = [fileN '_daily'];
end
%writeDir = ['/Volumes/projects/WiLMA/GLM/GLM run/sim/' lakeName '/'];
writeDir = '/Users/jread/Desktop/Science Projects/WiLMA/GLM files/';
infoDir  = '/Users/jread/Desktop/Science Projects/WiLMA/';
dataFormat = '%s,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f\n';

lakeIDs = {'1881900','805400','6100',...
    '7100','8000','8300','8600','8900',...
    '9400','9700','9800','10000','10300',...
    '10500','11500','11700','11900','12050',...
    '12070','12330','12370','13050','14400',...
    '15500','16600','18600','19900','22100',...
    '25100','25300','25600','27500'};
%lakeID = lakeID.(lakeName);
%% test script

fID = fopen([infoDir 'managed_lake_info.txt']);
dat = textscan(fID,'%s %s %s %f %s %f %f %f %f %f %f %s',...
'TreatAsEmpty','NA','HeaderLines',1,'Delimiter','\t');
fclose all;
wbic_i = 1;
lkeA_i = 4;
tic
load([rawDir fileN]);
toc


for lk = 1:length(lakeIDs)
    lakeID = lakeIDs{lk};
    
    lakeI= strcmp(GLM_NARR.feature,lakeID);
    
    wbicI = strcmp(dat{wbic_i},lakeID);
    lkeArea = dat{lkeA_i}(wbicI)*acre2m2; %in m2
    hc = 10; % GET THIS FROM COVER
    D = 2*sqrt(lkeArea/pi);
    Xt = 50*hc; % canopy height times 50
    
    Wstr = 2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2);
    Cu   = Wstr^0.33333;
    data = [GLM_NARR.SW(:,lakeI)*.90 ...
        GLM_NARR.LW(:,lakeI) ...
        GLM_NARR.airT(:,lakeI) ...
        GLM_NARR.RH(:,lakeI) ...
        GLM_NARR.wnd(:,lakeI)*Cu ...
        GLM_NARR.prcp(:,lakeI)*1.2/1000 ...
        GLM_NARR.snow(:,lakeI)/30000];
    dateWt = GLM_NARR.time;
    data = (interp2(dateWt,1:7,data',(dateWt(1):1/24:dateWt(end))',1:7))';
    dateWt = dateWt(1):1/24:dateWt(end);
    if daily
        fileName = [writeDir 'WBIC_' lakeID '_daily.csv'];
    else
        fileName = [writeDir 'WBIC_' lakeID '_hourly.csv'];
    end
    fid = fopen(fileName,'W+');
    headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
    fprintf(fid,headers);
    for j = 1:length(dateWt)
        fprintf(fid,dataFormat,datestr(dateWt(j),'yyyy-mm-dd HH:MM:SS'),data(j,:));
    end
    fclose all;
    disp(['done with ' fileName]);
    disp(['Cu scaling: ' num2str(Cu)]);
    toc
    disp('-----')
end







toc


end

