function writeGLMdriver2MAT


%lakeName  = 'Sparkling';
%lakeID = struct('Sparkling','1881900','Mendota','805400')

daily = false;

rawDir = '/Users/jread/Desktop/Science Projects/WiLMA/Driver data/';
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
yrs = 1980:2012;
%dataFormat = '%s,%2.1f,%2.1f,%2.1f,%2.1f,%2.1f,%2.5f,%2.5f\n';
%lakeID = lakeID.(lakeName);
% ASSUMES same lake IDs for all files!!!
%% test script
numTime = 100000;
numFeat = 3000;
initVar = NaN(numTime,numFeat);
varOut = struct('SW',initVar,'LW',initVar,'airT',initVar,'RH',initVar,...
    'wnd',initVar,'prcp',initVar,'snow',initVar,'time',NaN(numTime,1));
regVars = {'SW','LW','RH','prcp','snow'};

stIdx = struct('SW',1,'LW',1,'airT',1,'RH',1,...
    'wnd',1,'prcp',1,'snow',1,'time',1);
for yrI = 1:length(yrs)
    yr = num2str(yrs(yrI));
    
    %% non-regular vars --
    var = 'airT';
    tic
    [dates,data,lakeIDs] = wFileOpen([rawDir yr '/air_NARR.txt']);
    if eq(yrI,1)
        varOut.feature = lakeIDs;   % adding a field once
    end
    [nT,nF] = size(data);
    varOut.(var)(stIdx.(var):stIdx.(var)+nT-1,1:nF) = data;
    stIdx.(var) = stIdx.(var)+nT;
    varOut.time(stIdx.time:stIdx.time+nT-1,1) = dates;
    stIdx.time = stIdx.time+nT;
    
    var = 'wnd';
        %wnd
    [~,vwnd] = wFileOpen([rawDir yr '/vwnd_NARR.txt']);
    [~,uwnd] = wFileOpen([rawDir yr '/uwnd_NARR.txt']);
    data = sqrt(vwnd.^2+uwnd.^2);
    varOut.(var)(stIdx.(var):stIdx.(var)+nT-1,1:nF) = data;
    stIdx.(var) = stIdx.(var)+nT;
    
    %% regular vars
    for varI = 1:length(regVars)
        var = regVars{varI};
        [~,data] = wFileOpen([rawDir yr '/' var '_NARR.txt']);
        varOut.(var)(stIdx.(var):stIdx.(var)+nT-1,1:nF) = data;
        stIdx.(var) = stIdx.(var)+nT;
    end
  
    disp(['done with ' yr]);
    toc
end
% remove NaNs from structure

GLM_NARR = varOut;
fN = fieldnames(GLM_NARR);
for f = 1:length(fN);
    if ~strcmp(fN{f},'feature')
        vals = GLM_NARR.(fN{f});
        vals = vals(~isnan(vals(:,1)),:);   % remove NaN dates
        vals = vals(:,~isnan(vals(1,:)));   % remove NaN lakes
        GLM_NARR.(fN{f}) = vals(~isnan(vals(:,1)),:);
    end
end

fileN = 'GLM_GDP_driver';
if daily
    fileN = [fileN '_daily'];
end
save([saveDir fileN],'GLM_NARR','-v7.3');
disp('file saved')

toc


end

