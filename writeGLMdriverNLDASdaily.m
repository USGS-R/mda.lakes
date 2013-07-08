function writeGLMdriverNLDASdaily()



writeRoot = 'D:\WiLMA\Driver files\';
driverDir = 'D:\WiLMA\matYYYY\';

dataFormat = '%s,%2.1f,%2.1f,%2.2f,%2.1f,%2.2f,%2.3f,%2.3f\n';
dynamicVar = {'airT','LW','prcp','prss','spHum','SW','uWnd','vWnd'};
writeVar = {'ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow'};
%lakeIDs = getLakeIDs();
%%%% shortening lakeIDs %%%%
fID = fopen('D:\WiLMA\to_cal_wbic.csv');
lakeIDs = textscan(fID,'%s','HeaderLines',1,'Delimiter',',');
fclose(fID);
%lakeIDs = lakeIDs(1078:end);

kelvinConv = -273.15;
%lakeIDs = {'968800','805400'};


blcks = regexp(num2str(1979:2011),'  ','split');

%% block I: 
% - load block I data into memory
% - remove file if it exists
% - create a new file 
% - write first block of driver data 
% - clear memory of large loads
tic

for b = 1:length(blcks)
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
    for lk = 1:length(lakeIDs{1});
        lakeID = lakeIDs{1}{lk};
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
            wnd = sqrt(varStruct.uWnd(:,lakeI).^2+varStruct.vWnd(:,lakeI).^2); % hourly windspeed
            data(:,5) = wnd.^3;	% use for downsampling power (cubed)
            data(:,6) = varStruct.prcp(:,lakeI)*0.001; % now in m/hr
            
            snow = zeros(length(data(:,6)),1);
            swapI = lt(data(:,3),0);
            snow(swapI) = data(swapI,6)*10; % 10:1 ratio assuming 1:10 density ratio water weight
            data(swapI,6) = 0; % no precip falls as rain.
            
            data(:,7) = snow;
            if eq(b,1)
                fID = fopen([writeRoot fileN],'W'); % premission: Open or create new file for writing. Discard existing contents, if any.
                headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
            else
                fID = fopen([writeRoot fileN],'A');
                headers = '';
            end
            fprintf(fID,headers);
            
			dayDates = unique(floor(dates));
			dateCell = datestr(dayDates,'yyyy-mm-dd HH:MM:SS');
			
            for j = 1:length(dayDates)
				% downsample to daily
				useI = eq(floor(dates),dayDates(j));
				dataWrite = NaN(1,length(writeVar));
				for var = 1:length(writeVar)
					if strcmp(writeVar{var},'WindSpeed')
						dataWrite(:,var) = mean(data(useI,var))^0.33333;	% gets mean daily value of cube root
                    elseif strcmp(writeVar{var},'Rain')
						dataWrite(:,var) = sum(data(useI,var));	% gets mean daily value
                    elseif strcmp(writeVar{var},'Snow')
						dataWrite(:,var) = sum(data(useI,var));	% gets mean daily value
					else
						dataWrite(:,var) = mean(data(useI,var));	% gets mean daily value
					end
				end
                fprintf(fID,dataFormat,dateCell(j,:),dataWrite);
            end
            fclose(fID);
        else
            if eq(b,1)
                fID_log = fopen([driverDir '../WiLMA_driver_log.txt'],'A');
                fprintf(fID_log,[lakeID ' missing from driver set\n']);
                disp([lakeID ' missing from driver set, writing to log file'])
                fclose(fID_log);
            else
                disp([lakeID ' missing from driver set, skipping'])
            end
        end
        if eq(rem(lk,50),0)
            disp(['....' num2str(lk) ' lakes complete out of ' num2str(length(lakeIDs{1})) '....'])
        end
        
    end 
    disp(['**** done with block ' num2str(b) ' of ' num2str(length(blcks)) ...
        '(' blcks{b} ') ****'])
end

toc


end

