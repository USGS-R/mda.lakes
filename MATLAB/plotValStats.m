function plotValStats(runDir)

clc
close all

if eq(nargin,0)
    runDir = 'C:\Users\jread\Documents\WiLMA\GLM\Run\';
end

figDir = 'C:\Users\jread\Documents\WiLMA\Figures\';
fileName = 'cal.out.tsv';
disp(fileName);

colorChg = true;
useAlpha = false;
fAlpha = 0.15;
markDiv = 0.015; % size relative to axis;
minDep = 5; % min number of depths to calc a result from
surfMax = 1; % max depth to still be "surface"
mixDef = .5;
logColor = false;
rankArea = false;
slope = 0.1;
addpath('C:\Users\jread\Documents\MATLAB\Lake-Analyzer\Source')

folders = dir(runDir);

reader = '%s %s %f %f %f';


WBICs   = NaN(length(folders),1);
% dates   = dat{2};
% depths  = dat{3};
% wtrObs  = dat{4};
% wtrMod  = dat{5};
% fclose(fID);

%% arrays for calibration/validation metrics
SS      = NaN(length(WBICs)*100,2);
ThrmZ   = NaN(length(WBICs)*100,2);
EpiAve  = NaN(length(WBICs)*100,2);
HypAve  = NaN(length(WBICs)*100,2);
SurTemp = NaN(length(WBICs)*100,2);
AveTemp = NaN(length(WBICs)*100,2);
lkeSz   = NaN(length(WBICs)*100,1);
allTemp = NaN(length(WBICs)*1000,2);

cnt = 0;
atCnt = 0;
%% get unique lakes

for lk = 1:length(WBICs)
    % is it a folder?
    if folders(lk).isdir && gt(length(folders(lk).name),5)
        fID = fopen([runDir folders(lk).name '\' fileName]);
        if gt(fID,0)
            dat = textscan(fID,reader,'HeaderLines',1,'Delimiter','\t','TreatAsEmpty',{'NA'});
            fclose(fID);
            dates   = dat{2};
            depths  = dat{3};
            wtrObs  = dat{4};
            wtrMod  = dat{5};
            WBIC = folders(lk).name(6:end);
            bth = getBathy(WBIC);
            canopy = getCanopy(WBIC);
            lkeArea = getArea(WBIC);
            Wst = getWstr(canopy,lkeArea);
            bthA = bth(2,:);
            bthD = bth(1,:);
            tTime = dates;
            tWtrO = wtrObs;
            tWtrM = wtrMod;
            tDept = depths;
            unDates = unique(tTime);
            
            for j = 1:length(unDates)
                cnt = cnt+1;
                % need to test for duplicate values for depth...
                useI = strcmp(unDates{j},tTime);
                dep  = tDept(useI);
                [dep,dupI] = unique(dep,'first');
                wtrM = tWtrM(useI);
                wtrO = tWtrO(useI);
                wtrM = wtrM(dupI);
                wtrO = wtrO(dupI);
                allTemp(atCnt+1:atCnt+length(wtrM),2) = wtrM;
                allTemp(atCnt+1:atCnt+length(wtrM),1) = wtrO;
                rhoMod = waterDensity(wtrM);
                rhoObs = waterDensity(wtrO);
                atCnt = atCnt+length(wtrM);
                if ge(length(dep),minDep)
                    bI = length(rhoObs);
                    for d = 2:length(rhoObs)
                        if lt(rhoObs(d),rhoObs(d-1))
                            bI = d-1;
                            break
                        end
                    end
                    dep = dep(1:bI)';
                    wtrO = wtrO(1:bI)';
                    wtrM = wtrM(1:bI)';
                    nanI = isnan(wtrM);
                    dep = dep(~nanI);
                    wtrO = wtrO(~nanI);
                    wtrM = wtrM(~nanI);
                    if ge(length(dep),minDep)
                        % for obs
                        if le(max(wtrO)-min(wtrO),mixDef)
                            ThrmZ(cnt,1) = NaN;
                            EpiAve(cnt,1) = NaN;
                            HypAve(cnt,1) = NaN;
                        else
                            
                            [~,~,drho_dz,SthermoD] = ...
                                FindThermoDepth( rhoObs,dep);
                            metaTop = FindMetaTop(drho_dz,SthermoD,dep,slope);
                            metaBot = FindMetaBot(drho_dz,SthermoD,dep,slope);
                            ThrmZ(cnt,1) = SthermoD;
                            HypAve(cnt,1) = layerTemperature(metaBot,max(dep),wtrO,dep,bthA,bthD);
                            EpiAve(cnt,1) = layerTemperature(0,metaTop,wtrO,dep,bthA,bthD);
                        end
                        surI = ge(dep,surfMax);
                        if any(surI)
                            SurTemp(cnt,1) = mean(wtrO(surI));
                        end
                        AveTemp(cnt,1) = layerTemperature(0,max(dep),wtrO,dep,bthA,bthD);
                        
                        SS(cnt,1) = schmidtStability(wtrO,dep,bthA,bthD);
                        % for mod
                        if le(max(wtrM)-min(wtrM),mixDef)
                            ThrmZ(cnt,2) = NaN;
                            HypAve(cnt,2) = NaN;
                            EpiAve(cnt,2) = NaN;
                        else
                            [~,~,drho_dz,SthermoD] = ...
                                FindThermoDepth( rhoMod,dep);
                            metaTop = FindMetaTop(drho_dz,SthermoD,dep,slope);
                            metaBot = FindMetaBot(drho_dz,SthermoD,dep,slope);
                            HypAve(cnt,2) = layerTemperature(metaBot,max(dep),wtrM,dep,bthA,bthD);
                            EpiAve(cnt,2) = layerTemperature(0,metaTop,wtrM,dep,bthA,bthD);
                            ThrmZ(cnt,2) = SthermoD;
                        end
                        if any(surI)
                            SurTemp(cnt,2) = mean(wtrM(surI));
                        end
                        AveTemp(cnt,2) = layerTemperature(0,max(dep),wtrM,dep,bthA,bthD);
                        
                        SS(cnt,2) = schmidtStability(wtrM,dep,bthA,bthD);
                        if rankArea
                            lkeSz(cnt,1) = max(bthA);
                        else
                            lkeSz(cnt,1) = Wst;
                        end
                    end
                end
                
            end
            % unique lake, unique time.
        end
    end
    
    
    disp([num2str(lk) ' of ' num2str(length(WBICs))])
end
nanI = isnan(SS(:,1)) | isnan(SS(:,2));
SS = SS(~nanI,:);
EpiAve = EpiAve(~nanI,:);
HypAve = HypAve(~nanI,:);
ThrmZ =  ThrmZ(~nanI,:);
lkeSz = lkeSz(~nanI,:);


% all temps nanI
nanI = isnan(allTemp(:,1)) | isnan(allTemp(:,2));
allTemp = allTemp(~nanI,:);
%% create stats panel:

cmap = colormap(jet(100));
close all
if logColor 
    lkvals = linspace(log(min(lkeSz)),log(max(lkeSz)),100);
else
    lkvals = linspace(min(lkeSz),max(lkeSz),100);
end
    


figW = 7.5;
figH = 11;
lM = .75;
rM = .25;
bM = .75;
tM = .25;
vSpc = .7;
wSpc = .7;
mS = 1;
W = (figW-lM-rM-wSpc)/2;
H = (figH-tM-bM-vSpc*2)/3;
axLW = 1.0;
fontS = 12;
fontN = 'Times New Roman';


fig_h = figure('Color','w','Units','inches','Position',[0 0 figW figH],...
    'PaperSize',[figW figH],'PaperPosition',[0 0 figW figH]);

movegui(fig_h,'center');

ax_SS = axes('Parent',fig_h,'Position',[lM/figW (bM+vSpc+H)/figH W/figW H/figH],...
    'Box','on','LineWidth',axLW,'FontSize',fontS,'FontName',fontN);
hold on;

ax_ET = copyobj(ax_SS,fig_h);
set(ax_ET,'Position',[(lM+W+wSpc)/figW (bM+vSpc+H)/figH W/figW H/figH]);
set(get(ax_ET,'YLabel'),'String','Epilimnion temperature (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_ET,'XLabel'),'String','Epilimnion temperature (observed)',...
    'FontSize',fontS,'FontName',fontN);

ax_HT = copyobj(ax_SS,fig_h);
set(ax_HT,'Position',[(lM+W+wSpc)/figW bM/figH W/figW H/figH]);
set(get(ax_HT,'YLabel'),'String','Hypolimnion temperature (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_HT,'XLabel'),'String','Hypolimnion temperature (observed)',...
    'FontSize',fontS,'FontName',fontN);

ax_TD = copyobj(ax_SS,fig_h);
set(ax_TD,'Position',[lM/figW bM/figH W/figW H/figH]);
set(get(ax_TD,'YLabel'),'String','Thermocline depth (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_TD,'XLabel'),'String','Thermocline depth(observed)',...
    'FontSize',fontS,'FontName',fontN);

ax_AT = copyobj(ax_SS,fig_h);
set(ax_AT,'Position',[lM/figW (bM+2*vSpc+2*H)/figH W/figW H/figH]);
set(get(ax_AT,'YLabel'),'String','Average temperature (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_AT,'XLabel'),'String','Average temperature(observed)',...
    'FontSize',fontS,'FontName',fontN);

ax_ST = copyobj(ax_SS,fig_h);
set(ax_ST,'Position',[(lM+W+wSpc)/figW (bM+2*vSpc+2*H)/figH W/figW H/figH]);
set(get(ax_ST,'YLabel'),'String','Surface temperature (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_ST,'XLabel'),'String','Surface temperature(observed)',...
    'FontSize',fontS,'FontName',fontN);


set(get(ax_SS,'YLabel'),'String','Schmidt Stability (modeled)',...
    'FontSize',fontS,'FontName',fontN);
set(get(ax_SS,'XLabel'),'String','Schmidt Stability (observed)',...
    'FontSize',fontS,'FontName',fontN);




set(ax_SS,'Xlim',[0 2000],'YLim',[0 2000]);
set(ax_ET,'Xlim',[0 35],'YLim',[0 35]);
set(ax_HT,'Xlim',[0 35],'YLim',[0 35]);
set(ax_TD,'Xlim',[0 25],'YLim',[0 25]);
set(ax_ST,'Xlim',[0 35],'YLim',[0 35]);
set(ax_AT,'Xlim',[0 35],'YLim',[0 35]);


%% get stats:


res = SS(:,1)-SS(:,2);
SS_SE = sqrt(sum(res.^2)/length(res));
isN = isnan(EpiAve(:,1)) | isnan(EpiAve(:,2));
res= EpiAve(~isN,1)-EpiAve(~isN,2);
ET_SE = sqrt(sum(res.^2)/length(res));
isN = isnan(HypAve(:,1)) | isnan(HypAve(:,2));
res= HypAve(~isN,1)-HypAve(~isN,2);
HT_SE = sqrt(sum(res.^2)/length(res));
isN = isnan(ThrmZ(:,1)) | isnan(ThrmZ(:,2));
res= ThrmZ(~isN,1)-ThrmZ(~isN,2);
TD_SE = sqrt(sum(res.^2)/length(res));
isN = isnan(SurTemp(:,1)) | isnan(SurTemp(:,2));
res= SurTemp(~isN,1)-SurTemp(~isN,2);
ST_SE = sqrt(sum(res.^2)/length(res));
isN = isnan(AveTemp(:,1)) | isnan(AveTemp(:,2));
res= AveTemp(~isN,1)-AveTemp(~isN,2);
AT_SE = sqrt(sum(res.^2)/length(res));

bmp = .05;
format = '%2.1f';

yL = get(ax_SS,'YLim');
xL = get(ax_SS,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,SS_SE)],'Parent',ax_SS,'VerticalAlignment','top');
yL = get(ax_ET,'YLim');
xL = get(ax_ET,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,ET_SE)],'Parent',ax_ET,'VerticalAlignment','top');
yL = get(ax_HT,'YLim');
xL = get(ax_HT,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,HT_SE)],'Parent',ax_HT,'VerticalAlignment','top');
yL = get(ax_TD,'YLim');
xL = get(ax_TD,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,TD_SE)],'Parent',ax_TD,'VerticalAlignment','top');
yL = get(ax_ST,'YLim');
xL = get(ax_ST,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,ST_SE)],'Parent',ax_ST,'VerticalAlignment','top');
yL = get(ax_AT,'YLim');
xL = get(ax_AT,'XLim');
text(xL(2)*bmp,yL(2)*(1-bmp),['SE:' sprintf(format,AT_SE)],'Parent',ax_AT,'VerticalAlignment','top');

for lk = 1:length(lkeSz)
    if le(lkeSz(lk),lkvals(length(lkvals))) % now, no cutoff
        clr = [.2 .2 .2];
        if colorChg
            for i = 1:3
                if logColor
                    clr(i) = interp1(lkvals,cmap(:,i),log(lkeSz(lk)));
                else
                    clr(i) = interp1(lkvals,cmap(:,i),lkeSz(lk));
                end
                
            end
        end
        
        if useAlpha
            % Schmidt
            xL = get(ax_SS,'XLim'); yL = get(ax_SS,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = SS;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_SS,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
            
            % Epilimnion
            xL = get(ax_ET,'XLim'); yL = get(ax_ET,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = EpiAve;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_ET,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
            
            % Hypo
            xL = get(ax_HT,'XLim'); yL = get(ax_HT,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = HypAve;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_HT,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
            
            % Thermo depth
            xL = get(ax_TD,'XLim'); yL = get(ax_TD,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = ThrmZ;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_TD,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
            
            % Ave temp
            xL = get(ax_HT,'XLim'); yL = get(ax_AT,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = AveTemp;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_AT,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
            
            % Surface temp
            xL = get(ax_HT,'XLim'); yL = get(ax_ST,'YLim');
            bX = markDiv*(xL(2)-xL(1));
            bY = markDiv*(yL(2)-yL(1));
            plVal = SurTemp;
            
            fill([plVal(lk,1) plVal(lk,1)+bX*.5 plVal(lk,1) plVal(lk,1)-bX*.5],...
                [plVal(lk,2)+bY*.5 plVal(lk,2) plVal(lk,2)-bY*.5 plVal(lk,2)],'r',...
                'Parent',ax_ST,'EdgeColor','none','FaceAlpha',fAlpha,...
                'FaceColor',clr);
        else
            plot(SS(lk,1),SS(lk,2),'ro','MarkerSize',mS,'Parent',ax_SS,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
            plot(EpiAve(lk,1),EpiAve(lk,2),'ro','MarkerSize',mS,'Parent',ax_ET,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
            plot(HypAve(lk,1),HypAve(lk,2),'ro','MarkerSize',mS,'Parent',ax_HT,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
            plot(ThrmZ(lk,1),ThrmZ(lk,2),'ro','MarkerSize',mS,'Parent',ax_TD,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
            plot(AveTemp(lk,1),AveTemp(lk,2),'ro','MarkerSize',mS,'Parent',ax_AT,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
            plot(SurTemp(lk,1),SurTemp(lk,2),'ro','MarkerSize',mS,'Parent',ax_ST,...
                'MarkerEdgeColor',clr,'MarkerFaceColor',clr);
        end
    end
end
print('-dpng','-r300',[figDir 'calVal.png'])
end

