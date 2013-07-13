function buildIceOffCalibration

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
writeRoot = '';
driverRoot = 'D:\WiLMA\Driver files\';
UTC = -6;   % get actual UTC offset for daylight savings!   
%% open ice cover file and extract necessary information
fID = fopen('supporting files/Validation/ice_data.csv');

close all
clc
dat = textscan(fID,'%f %f %f %f %f %f %f %s %s %f %f %s %s','Delimiter',',',...
    'HeaderLines',1);

fclose(fID);

WBICs = dat{13};
iceOffYYYY  = dat{4};
iceOffMM    = dat{5};
iceOffdd    = dat{6};
JulDay = datenum(iceOffYYYY,iceOffMM,iceOffdd)-datenum(iceOffYYYY,0,0);
rmvI = lt(JulDay,0) | lt(iceOffYYYY,0) | ...
    strcmp(WBICs,'1864700') | strcmp(WBICs,'1869700') | ...
    strcmp(WBICs,'234000') | strcmp(WBICs,'968500'); % don't have that WBIC
JulDay = JulDay(~rmvI);
iceOffYYYY = iceOffYYYY(~rmvI);
WBICs  = WBICs(~rmvI);
unWBICs = unique(WBICs);

dataFormat = '%s\t%3.0f\t%3.0f\t%2.3f\t%2.3f\t%2.3f\t%2.3f\n';

fileName = [writeRoot 'iceModel.tsv'];
fid = fopen(fileName,'W');
headers = 'WBIC\ticeOff(jDay)\tzero_sp\tang_sp\tSA\tlong\tEL\tswSmooth\n';
fprintf(fid,headers);
fclose(fid);
figure;

%% now get all info


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
    sw = dat{2};
    smthAir = airT*NaN;
    smthSW = sw*NaN;
    % create 30 day smoothed air temp. Assumed centered left!
    for j = 16:length(airT)-14
        smthAir(j) = mean(airT(j-15:j+14));
        smthSW(j) = mean(sw(j-15:j+14));
    end
    
    % ---- get zero_sp day
    useI = strcmp(unWBICs{i},WBICs);
    julUse = JulDay(useI);
    yrUse = iceOffYYYY(useI);
    zero_sp = julUse*NaN;
    swSmooth = julUse*NaN;
    ang_sp = julUse*NaN;
    indx = 1:length(dates);
    % for each ice off year, find the starting point to move back from
    [lat, lon] = getLatLon(WBIC);
    SA = getArea(WBIC)*1e-6;
    EL = getElev(WBIC);
    fid = fopen(fileName,'A');
    for j = 1:length(julUse);
        startI = indx(eq(dates,datenum(yrUse(j),0,0)+183));

        zero_sp(j) = find(le(smthAir(startI-183:startI),0),1,'last')+1; % this is the day
        swSmooth(j) = smthSW(find(le(smthAir(startI-183:startI),0),1,'last')+1);
        dateV = datevec(datenum(yrUse(j),0,0)+zero_sp(j));
        
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
        ang_sp(j) = sun.zenith;
        hold on;
        %modIceOff = 175.829+0.25676*zero_sp(j)-2.9453*ang_sp(j)+0.0009347*SA+0.49134*lon+0.01691*EL;
        modIceOff = -325.51345+1.82236*zero_sp(j)+2.95482*ang_sp(j)+0.03313*SA-1.48418*lon+0.02767*EL;
        plot(julUse(j),modIceOff,'ro','markerSize',j+4);
        fprintf(fid,dataFormat,WBIC,[julUse(j),zero_sp(j),ang_sp(j),SA,lon,EL,swSmooth(j)]);
        pause(0.1);
    end
    fclose(fid);
    
end
    
fclose all;




end

