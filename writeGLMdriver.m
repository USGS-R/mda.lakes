function writeGLMdriver()


daily = false;

rawDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';
fileN = 'GLM_GDP_driver';
if daily 
    rawDir = [rawDir 'Dailies/'];
    fileN = [fileN '_daily'];
end

dataFormat = '%s,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f\n';
lakeIDs = getLakeIDs();

tic
load([rawDir fileN]);
toc


for lk = 27:length(lakeIDs)
    lakeID = lakeIDs{lk};
    
    lakeI= strcmp(GLM_NARR.feature,lakeID);
    Kd = getClarity(lakeID);
    bth= getBathy(lakeID);
    if any(lakeI) && ~any(isnan(bth(1,:))) % otherwise, skip
        lakeRef = ['WBIC_' lakeID];
        writeDir = ['/Volumes/projects/WiLMA/GLM/GLM run/sim/' lakeRef '/'];
        mkdir(writeDir);
        lkeArea = getArea(lakeID);
        hc = 10; % GET THIS FROM COVER
        D = 2*sqrt(lkeArea/pi);
        Xt = 50*hc; % canopy height times 50
        
        Wstr = 2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2);
        if le(D,Xt)
            Wstr = 0.0005;
        end
        wnd = GLM_NARR.wnd(:,lakeI);
        wnd = wnd.^1.4;
        Cu   = Wstr^0.33333;
        data = [GLM_NARR.SW(:,lakeI)*0.85 ...
            GLM_NARR.LW(:,lakeI) ...
            GLM_NARR.airT(:,lakeI)-273.15 ...
            GLM_NARR.RH(:,lakeI) ...
            wnd*Cu ...
            GLM_NARR.prcp(:,lakeI)/50 ...
            GLM_NARR.snow(:,lakeI)/30000];
        dateWt = GLM_NARR.time;
        data = (interp2(dateWt,1:7,data',(dateWt(1):1/24:dateWt(end))',1:7))';
        dateWt = dateWt(1):1/24:dateWt(end);
        
        if daily
            metFile = [lakeRef '_daily.csv'];
        else
            metFile = [lakeRef '_hourly.csv'];
        end
        fileName = [writeDir metFile];
        fid = fopen(fileName,'W+');
        headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
        fprintf(fid,headers);
        for j = 1:length(dateWt)
            fprintf(fid,dataFormat,datestr(dateWt(j),'yyyy-mm-dd HH:MM:SS'),data(j,:));
        end
        fclose all;
        disp(['done with driver data for ' lakeRef]);
        disp(['Cu scaling: ' num2str(Cu)]);
        %% now build the param file
        
        
        if isnan(Kd)
            Kd = 1;
        end
        elev = 400;
        bthH = bth(1,:);
        bthA = bth(2,:);
        for i = 1:length(bth(1,:))
            bthH(i) = elev-bth(1,length(bthH)-i+1);
            bthA(i) = bth(2,length(bthA)-i+1)/1000; %% FIXX
        end
        
        writeGLMnmlParamFile('Kw_FLT',Kd,'lake_name_STR',lakeRef,...
            'latitude_FLT',43,'longitude_FLT',-89,...
            'H_csvVEC',bthH,'A_csvVEC',bthA,'meteo_fl_STR',metFile,...
            'wind_factor_FLT',1,'ce_FLT',0.0018,'ch_FLT',0.0013/(Cu^2),...
            'stop_STR','2012-06-30 00:00:00','min_layer_thick_FLT',0.5,...
            'max_layer_thick_FLT',0.5)
        toc
        disp(['lake ' num2str(lk) ' of ' num2str(length(lakeIDs))]);
        disp('-----');
    else
        disp(['skipping ' lakeID]);
        disp('-----');
    end
end







toc


end

