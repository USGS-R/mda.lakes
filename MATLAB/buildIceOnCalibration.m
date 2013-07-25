function buildIceOnCalibration

% writes to a file that I will use R to read and apply lm() to.

% do any of these lake years have ice-off that occurs before Jan 1st???

% vars needed:
% - ice off date (julian day)
% - daily air temperature for that year
% - sun angle (requires lat and long)
% - surface area of lake
% - longitude of lake
% - elevation of lake

% ==== need to subsample for validation (elsewhere) ====

%% variables
writeRoot = '..\supporting files\Validation\';
driverRoot = 'D:\WiLMA\Driver files\';
UTC = -6;   % get actual UTC offset for daylight savings!   
%% open ice cover file and extract necessary information
fID = fopen('../supporting files/Validation/ice_data.csv');

close all
clc
dat = textscan(fID,'%f %f %f %f %f %f %f %s %s %f %f %s %s','Delimiter',',',...
    'HeaderLines',1);

fclose(fID);

WBICs = dat{13};
iceOnYYYY  = dat{1};
iceOnMM    = dat{2};
iceOndd    = dat{3};
JulDay = datenum(iceOnYYYY,iceOnMM,iceOndd)-datenum(iceOnYYYY,0,0);

% special fix for jan/feb ice-on dates!!!
flipI = lt(JulDay,70) & gt(JulDay,0);
JulDay(flipI) = JulDay(flipI)+365;

rmvI = lt(JulDay,0) | lt(iceOnYYYY,0) | ...
    strcmp(WBICs,'1864700') | strcmp(WBICs,'1869700') | ...
    strcmp(WBICs,'234000') | strcmp(WBICs,'968500'); % don't have that WBIC
JulDay = JulDay(~rmvI);
iceOnYYYY = iceOnYYYY(~rmvI);
WBICs  = WBICs(~rmvI);
unWBICs = unique(WBICs);

dataFormat = '%s\t%3.0f\t%3.0f\t%2.3f\t%2.3f\t%2.3f\r\n';

% zero_fl: day of year when 30 day smoothed temp first reaches 0 in fall
% ang_fl: angle of sun on zero_fl
% AT_fl: average temperature for the 3 month period that contains zero_fl
% mn_dp: mean depth of lake (m)
% 
fileName = [writeRoot 'iceModel_on.tsv'];
fid = fopen(fileName,'W');
headers = 'WBIC\ticeOn(jDay)\tzero_fl\tang_fl\tAT_fl\tmn_dp\r\n';
fprintf(fid,headers);
fclose(fid);

%% now get all infosd


for i = 1:length(unWBICs);
    WBIC = unWBICs{i};
    % open driver file
    fID = fopen([driverRoot 'WBIC_' WBIC '.csv']);
    if lt(fID,0)
        disp(WBIC)
    end
    dat = textscan(fID,'%s %f %f %f %f %f %f %f %f',...
        'Delimiter',',','HeaderLines',1);
    fclose(fID);
    dates = datenum(dat{1});
    airT = dat{4};
    smthAir = airT*NaN;
    % create 30 day smoothed air temp. Assumed centered left!
    for j = 16:length(airT)-14
        smthAir(j) = mean(airT(j-15:j+14));
    end
    
    % ---- get zero_sp day
    useI = strcmp(unWBICs{i},WBICs);
    julUse = JulDay(useI);
    yrUse = iceOnYYYY(useI);
    zero_fl = julUse*NaN;
    ang_fl = julUse*NaN;
    AT_fl = julUse*NaN;
    indx = 1:length(dates);
    % for each ice off year, find the starting point to move back from
    [lat, lon] = getLatLon(WBIC);
    EL = getElev(WBIC);
    mn_dp = getZmean(WBIC);
    fid = fopen(fileName,'A');
    for j = 1:length(julUse);
        startI = indx(eq(dates,datenum(yrUse(j),0,0)+183));
        lasI = min(startI+220,length(dates));
        flDay = find(le(smthAir(startI:lasI),0),1,'first')+183; % this is the day
        if ~isempty(flDay)
            zero_fl(j) = flDay;
        else
            zero_fl(j) = lasI;
        end
        dateV = datevec(datenum(yrUse(j),0,0)+zero_fl(j));
        
        location.longitude = lon;
        location.latitude = lat;
        location.altitude = EL;
        time.year = dateV(1);
        time.month = dateV(2);
        time.day = dateV(3);
        time.hour = 12;
        time.min = 0;
        time.sec = 0;
        time.UTC = UTC;
        sun = sun_position(time, location);
        ang_fl(j) = sun.zenith;
        
        winAT_1 = datenum(yrUse(j),0,0)+zero_fl(j)-30; % window for ave 3 monthly temps
        winAT_2 = datenum(yrUse(j),0,0)+zero_fl(j)+60;
        useI = ge(dates,winAT_1) & lt(dates,winAT_2);
        AT_fl(j) = mean(airT(useI));
        modIceOn = 57.95749+zero_fl(j)*1.47585+ang_fl(j)*-3.38068+8.99045*mn_dp^0.5;
        plot(julUse(j),modIceOn,'ro','markerSize',j+4);
        hold on;
        %modIceOff = 175.829+0.25676*zero_sp(j)-2.9453*ang_sp(j)+0.0009347*SA+0.49134*lon+0.01691*EL;
        %modIceOff = -325.51345+1.82236*zero_sp(j)+2.95482*ang_sp(j)+0.03313*SA-1.48418*lon+0.02767*EL;
        fprintf(fid,dataFormat,WBIC,[julUse(j),zero_fl(j),ang_fl(j),AT_fl(j),mn_dp]);
        pause(0.01);
    end
    fclose(fid);
    
end
    
fclose all;




end

