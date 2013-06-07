function writeNLDASdriver2MAT


%lakeName  = 'Sparkling';
%lakeID = struct('Sparkling','1881900','Mendota','805400')

daily = false;

rawDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/NLDAS/';
if daily 
    rawDir = [rawDir 'Dailies/'];
end
saveDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';

% will need to aggregate across files, but for now, just do one year.
% write format: csv with date, SW, LW, CC%, airT, RH, wnd, rain, Snow
% CC% is (1-H/Ho*100); for Ho: getSWFromTime.m
% RH is VP/satVP; for satVP: SatVaporFromTemp.m
% wnd needs wind model overlay (lake specific)

% -- need latitude from feature --

%dataFormat = '%s,%2.1f,%2.1f,%2.1f,%2.1f,%2.1f,%2.5f,%2.5f\n';
%lakeID = lakeID.(lakeName);
% ASSUMES same lake IDs for all files!!!
%% test script
tic
varNames = {'SW','Downward_shortwave_radiation_flux_surface';...
    'LW','Downward_longwave_radiation_flux_surface';...
    'airT','Temperature_height_above_ground';...
    'uWnd','u-component_of_wind_height_above_ground';...
    'vWnd','v-component_of_wind_height_above_ground';...
    'prss','Pressure_surface';...
    'spHum','Specific_humidity_height_above_ground';...
    'prcp','Total_precipitation_surface_1_Hour_Accumulation'};
% grab var by vars

fID =  fopen([rawDir 'NLDAS_2000_2009.tsv']);

blckN = cell(10,1);
blck  = NaN(10,1);
lneInd = 0;
blkInd = 0;
endOfFile = feof(fID);
tic
while ~endOfFile
    lneInd = lneInd+1;
    lineT =fgets(fID);
    if strcmp(lineT(1),'#')
        blkInd = blkInd+1;
        blck(blkInd) = lneInd;
        text = lineT(3:end);
        text(isspace(text)) = [];
        blckN{blkInd} = text;
        disp(text)
        disp(' ')
        toc
    end
    endOfFile = feof(fID);
    if endOfFile
        blck(blkInd+1) = lneInd;
    end
    if eq(blkInd,3)  %%%%%%%%% REMOVE THIS %%%%%%%%%%
        break
    end
end
disp(blck)
disp(blckN)
%blckN = blckN{~isempty(blckN)};
blck  = blck(~isnan(blck));
fseek(fID,0,-1); 
[~] = fgets(fID);
names = regexp(fgets(fID),'\t','split');
names = names(2:end);
numFeat = length(names);

reader = blanks((numFeat+1)*3);
reader2 = blanks((numFeat+1)*3+1); % for height
reader(1:3) = '%s ';
reader2(1:5) = '%s %s';

for i = 4:3:length(reader)-2
    reader(i:i+2) = '%f ';
    reader2(i+3:i+5) = '%f '; %for "above_ground"
end


% now we have all names of blocks and the block starting points. 
for b = 1:2%length(blck)-1 % FIX !!!!!!----- set to 1
    fseek(fID,0,-1); 
    if strfind(blckN{b},'above_ground')
        dat = textscan(fID,reader2,blck(b+1)-(blck(b)+3),'Delimiter','\t','HeaderLines',blck(b)+2);
        data = cell2mat(dat(3:end)); % skip the "height"
    else
        dat = textscan(fID,reader,blck(b+1)-(blck(b)+3),'Delimiter','\t','HeaderLines',blck(b)+2);
        data = cell2mat(dat(2:end));
    end
    
    timeRaw = dat{1}(1:length(data(1:end,1)));
    timeRaw = regexprep(regexprep(timeRaw,'T',' '),'Z','');
    time = datenum(timeRaw,'yyyy-mm-dd HH:MM:SS' );
    useN = varNames(strcmp(varNames(1:end,2),blckN{b}),1);
    disp(['saving ' useN{1} ' NLDAS...']);
    disp(['data size:' num2str(size(data)) ', time size:'...
        num2str(size(time)) ', names size:' num2str(length(names))])
    save(['NLDAS_' useN{1} '_' datestr(time(1),'yyyy') ...
        '-' datestr(time(end),'yyyy')],'names', 'time', 'data')
end


% will break at NaN

end
