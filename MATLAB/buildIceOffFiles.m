function buildIceOffFiles

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
writeRoot = 'supporting files\Validation\';
driverRoot = 'D:\WiLMA\Driver files\';
UTC = -6;

%%

WBICs  = getLakeIDs();
unWBICs = unique(WBICs);

dataFormat = '%s\t%4.0f\t%3.0f\t%2.3f\t%2.3f\t%2.3f\t%2.3f\t%3.1f\t%3.2f\t%2.3f\t%3.0f\r\n';

fileName = [writeRoot 'iceModel_onoff.tsv'];
fid = fopen(fileName,'W');
headers = 'WBIC\tYYYY\tzero_sp\tang_sp\tSA\tlong\tEL\tswSmooth\tmxDepth\tang_fl\tzero_fl\r\n';
fprintf(fid,headers);
fclose(fid);

%% now get all infosd


for i = 1:length(unWBICs);
    WBIC = unWBICs{i};
    % open driver file
    fID = fopen([driverRoot 'WBIC_' WBIC '.csv']);
    if lt(fID,0)
        disp(WBIC)
    else
        dat = textscan(fID,'%s %f %f %f %f %f %f %f %f',...
            'Delimiter',',','HeaderLines',1);
        fclose(fID);
        dates = datenum(dat{1});
        airT = dat{4};
        sw = dat{2};
        smthAir = airT*0-999;
        smthSW = sw*0-999;
        % create 30 day smoothed air temp. Assumed centered left!
        for j = 16:length(airT)-14
            smthAir(j) = mean(airT(j-15:j+14));
            smthSW(j) = mean(sw(j:j+14));
        end
        
        % ---- get zero_sp day
        yrs = 1979:2011;
        zero_sp = yrs*NaN;
        zero_fl = yrs*NaN;
        swSmooth = yrs*NaN;
        ang_sp = yrs*NaN;
        ang_fl = yrs*NaN;
        indx = 1:length(dates);
        % for each ice off year, find the starting point to move back from
        [lat, lon] = getLatLon(WBIC);
        bth = getBathy(WBIC);
        mxDepth = bth(1,end);
        SA = getArea(WBIC)*1e-6;
        EL = getElev(WBIC);
        fid = fopen(fileName,'A');
        for j = 1:length(yrs);
            startI = indx(eq(dates,datenum(yrs(j),0,0)+183));
            firI = max(1,startI-183);
            lasI = min(startI+220,length(dates));
            zero_sp(j) = find(le(smthAir(firI:startI),0),1,'last')+1; % this is the day
            flDay = find(le(smthAir(startI:lasI),0),1,'first')+183; % this is the day
            if ~isempty(flDay)
                zero_fl(j) = flDay;
            else
                zero_fl(j) = lasI;
            end
            swSmooth(j) = smthSW(find(le(smthAir(firI:startI),0),1,'last')+1);
            dateV = datevec(datenum(yrs(j),0,0)+zero_sp(j));
            
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
            
            dateV = datevec(datenum(yrs(j),0,0)+zero_fl(j));
            time.year = dateV(1);
            time.month = dateV(2);
            time.day = dateV(3);
            sun = sun_position(time, location);
            ang_fl(j) = sun.zenith;
            %modIceOff = 175.829+0.25676*zero_sp(j)-2.9453*ang_sp(j)+0.0009347*SA+0.49134*lon+0.01691*EL;
            %modIceOff = -325.51345+1.82236*zero_sp(j)+2.95482*ang_sp(j)+0.03313*SA-1.48418*lon+0.02767*EL;
            %plot(julUse(j),modIceOff,'ro','markerSize',j+4);
            fprintf(fid,dataFormat,WBIC,[yrs(j),zero_sp(j),ang_sp(j),SA,lon,EL,swSmooth(j),mxDepth,ang_fl(j),zero_fl(j)]);
            %pause(0.01);
        end
        fclose(fid);
    end
    disp(['done with ' WBIC])
    disp([num2str(i) ' of ' num2str(length(unWBICs))])
    
end
    
fclose all;




end

