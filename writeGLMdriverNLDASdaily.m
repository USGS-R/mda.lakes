function writeGLMdriverNLDASdaily()



writeRoot = '/Users/jread/Desktop/Science Projects/WiLMA/GLM files/Driver files/';
driverDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/NLDAS/matYYYY/';

dataFormat = '%s,%2.1f,%2.1f,%2.2f,%2.1f,%2.2f,%2.3f,%2.3f\n';
dynamicVar = {'airT','LW','prcp','prss','spHum','SW','uWnd','vWnd'};
writeVar = {'ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'};
lakeIDs = getLakeIDs();
%%%% shortening lakeIDs %%%%
%lakeIDs = lakeIDs(1078:end);

kelvinConv = -273.15;
%lakeIDs = {'968800','805400','190500','182000'};
tic
load([driverDir '../../StationaryGLM_struct']);
% loads Hc, snow from NARR
toc

blcks = regexp(num2str(1979:2011),'  ','split');

%% block I: 
% - load block I data into memory
% - remove file if it exists
% - create a new file 
% - write first block of driver data 
% - clear memory of large loads
tic

for b = 33:length(blcks)
    % load all into memory:
    disp(' ')
    toc
    for v = 1:length(dynamicVar)
        varIn = load([driverDir dynamicVar{v} '_' blcks{b} '_NLDAS.mat']);
        varStruct.(dynamicVar{v}) = varIn.var;
        names = varIn.names;
        dates = varIn.dates;
        clear varIn;
    end
    % now, lake loop
    
    disp(varStruct)
    if eq(b,11) || eq(b,21) || eq(b,31) || eq(b,33)
        varStruct.prcp = [varStruct.prcp(:,:); zeros(length(names),1)'];
        disp(['>> precip added one zero in ' blcks{b}])
    end
    for lk = 1:length(lakeIDs);
        lakeID = lakeIDs{lk};
        fileN = ['WBIC_' lakeID '.csv'];
        lakeI= strcmp(names,lakeID);
        if any(lakeI)
            data = zeros(length(dates),length(writeVar)); % NEED TO CONVERT, and call SNOW!!!!
            data(:,1) = varStruct.SW(:,lakeI);
            data(:,2) = varStruct.LW(:,lakeI);
            data(:,3) = varStruct.airT(:,lakeI)+kelvinConv;
            Q = varStruct.spHum(:,lakeI);
            q_sat = qsat(data(:,3),varStruct.prss(:,lakeI)*0.01);
            RH = 100*Q./q_sat;
            data(:,4) = RH;
            lkeArea = getArea(lakeID);
            wnd = sqrt(varStruct.uWnd(:,lakeI).^2+varStruct.vWnd(:,lakeI).^2); % hourly windspeed
			wndPwr = wnd.^3;	% use for downsampling power (cubed)
			
            structI = strcmp(StationaryGLM_struct.feature,lakeID);
            hc = max(str2double(StationaryGLM_struct.Hc{structI}),1);   % must be at least 1 m;
            D = 2*sqrt(lkeArea/pi);
            Xt = 50*hc; % canopy height times 50
            
            Wstr = 2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2);
            if le(D,Xt)
                Wstr = 0.0005;
            end
            wnd = wnd.^1.3;
            Cu   = Wstr^0.33333;
            data(:,5) = Cu*wnd;
            data(:,6) = varStruct.prcp(:,lakeI)*0.001; % now in m/hr
            
            snow = zeros(length(data(:,6)),1);
            swapI = lt(data(:,3),0);
            snow(swapI) = data(swapI,6)*10; % 10:1 ratio
            % find snow to repace timesteps:
            %useI = ismember(StationaryGLM_struct.time,dates);
            %swapI = ismember(dates,StationaryGLM_struct.time);
            %snow(swapI) = StationaryGLM_struct.snow(useI,structI);
            %snow = snow*0.01; % assuming 1:10 density ratio water weight % is WRONG?????
            
            data(:,7) = snow;
            if eq(b,1)
                fID = fopen([writeRoot fileN],'W'); % premission: Open or create new file for writing. Discard existing contents, if any.
                headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
            else
                fID = fopen([writeRoot fileN],'A');
                headers = '';
            end
            fprintf(fID,headers);
            dateCell = datestr(dates,'yyyy-mm-dd HH:MM:SS');
            for j = 1:length(dates)
                fprintf(fID,dataFormat,dateCell(j,:),data(j,:));
            end
            fclose(fID);
        else
            if eq(b,1)
                fID_log = fopen([driverDir '../../../WiLMA_driver_log.txt'],'A');
                fprintf(fID_log,[lakeID ' missing from driver set\n']);
                disp([lakeID ' missing from driver set, writing to log file'])
                fclose(fID_log);
            else
                disp([lakeID ' missing from driver set, skipping'])
            end
        end
        if eq(rem(lk,10),0)
            disp(['....' num2str(lk) ' lakes complete out of ' num2str(length(lakeIDs)) '....'])
        end
        
    end 
    disp(['**** done with block ' num2str(b) ' of ' num2str(length(blcks)) ...
        '(' blcks{b} ') ****'])
end
fclose all;
% 
%         %% now build the param file
%         

    lakeID = '1008700'
        Kd = getClarity(lakeID);
        bth= getBathy(lakeID);
        if isnan(Kd)
            Kd = 1;
        end
        elev = getElev(lakeID);
        bthH = bth(1,:);
        bthA = bth(2,:);
        for i = 1:length(bth(1,:))
            bthH(i) = elev-bth(1,length(bthH)-i+1);
            bthA(i) = bth(2,length(bthA)-i+1)/1000; %% FIXX
        end
        lakeRef = ['WBIC_' lakeID];
        metFile = ['WBIC_' lakeID '.csv'];
        writeGLMnmlParamFile('Kw_FLT',Kd,'lake_name_STR',lakeRef,...
            'latitude_FLT',43,'longitude_FLT',-89,...
            'H_csvVEC',bthH,'A_csvVEC',bthA,'meteo_fl_STR',metFile,...
            'wind_factor_FLT',1,'ce_FLT',0.0013/Cu,'ch_FLT',0.0013/Cu,...
            'stop_STR','2012-06-30 00:00:00','min_layer_thick_FLT',0.5,...
            'max_layer_thick_FLT',0.5)
        toc
        disp(['lake ' num2str(lk) ' of ' num2str(length(lakeIDs))]);
        disp('-----');
%     else
%         disp(['skipping ' lakeID]);
%         disp('-----');
%     end
% end
% 
% 
% 
% 
% 


toc


end

