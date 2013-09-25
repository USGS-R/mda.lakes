function plotHistJulyGLM

clc
close all
rootDir = '/Users/jread/Desktop/Science Projects/WiLMA/Results data/';
figDir = '/Users/jread/Desktop/Science Projects/WiLMA/Figures/';

fID = fopen([rootDir 'allLakesJulySurf.tsv']);
dat = textscan(fID,'%f %f %s %f','HeaderLines',1,'Delimiter','\t','TreatAsEmpty','NA');

fclose all;

wtrS = dat{4};
year = dat{2};

rmvI = isnan(wtrS);
wtrS = wtrS(~rmvI);
year = year(~rmvI);

years = unique(year);

%% figure 1, single year
fontS = 14;
fontN = 'Cambria';
figW = 5.5;
figH = 5;
tM = 0.12;
lM = 0.55;
rM = 0.12;
bM = .75; 
W = figW-rM-lM;
H = figH-bM-tM;
yL = [0 10];
yTck = yL(1):2:yL(2);
xL = [15 32];
xTck = 2:2:xL(2);
tL = [0.005 0];
binX = xL(1):0.4:xL(2);

fig_h = figure('Color','w','Units','inches','Position',[0 0 figW figH],'PaperPosition',[0 0 figW figH]);
movegui(fig_h,'center');

ax_h = axes('Parent',fig_h,'Position',[lM/figW bM/figH W/figW H/figH],...
    'Box','on','LineWidth',1,'TickDir','out',...
    'YLim',yL,'XLim',xL,'YTick',yTck,'XTick',xTck,'FontName',...
    fontN,'FontSize',fontS,'TickLength',tL);

xlabel('Mean July surface temperature (°C)','FontName',fontN,'FontSize',fontS)
ylabel('Frequency of observation (%)','FontName',fontN,'FontSize',fontS)
hold on;

medianT = zeros(length(years),1);
yr = 2009;
useI = eq(yr,year);
tWtr = wtrS(useI);
cnt = zeros(length(binX)-1,1);
for b = 2:length(binX);
    left = binX(b-1);
    right = binX(b);
    cnt(b-1) = sum(ge(tWtr,left) & lt(tWtr,right));
    
end
medianT = median(tWtr(~isnan(tWtr)));
cnt(eq(cnt,0)) = NaN;
totalC = sum(~isnan(tWtr));
plot(binX(1:end-1),cnt/totalC*100,'k-','LineWidth',1.25,'Parent',ax_h)
medVal = interp1(binX(1:end-1),cnt/totalC*100,medianT);

print([figDir 'WiLMA_July_' num2str(yr)],'-dpng','-r200')
plot(medianT,medVal,'ro','Color',[.7 .2 .2],'LineWidth',1.25,'MarkerFaceColor',[.7 .2 .2]);
% do counts here
print([figDir 'WiLMA_July_wMed_' num2str(yr)],'-dpng','-r200')
disp(medianT)

plot(binX(1:end-1),cnt/totalC*100,'k-','LineWidth',1.25,'Parent',ax_h,'Color',[.7 .7 .7])
plot(medianT,medVal,'ro','Color',[.6 .6 .6],'LineWidth',1.25,'MarkerFaceColor',[.6 .6 .6]);

tWtr = wtrS;
cnt = zeros(length(binX)-1,1);
for b = 2:length(binX);
    left = binX(b-1);
    right = binX(b);
    cnt(b-1) = sum(ge(tWtr,left) & lt(tWtr,right));
    
end
medianT = median(tWtr(~isnan(tWtr)));
cnt(eq(cnt,0)) = NaN;
totalC = sum(~isnan(tWtr));
medVal = interp1(binX(1:end-1),cnt/totalC*100,medianT);

plot(binX(1:end-1),cnt/totalC*100,'k-','LineWidth',1.25,'Parent',ax_h)
plot(medianT,medVal,'ro','Color',[.7 .2 .2],'LineWidth',1.25,'MarkerFaceColor',[.7 .2 .2]);
disp(medianT)
print([figDir 'WiLMA_July_All'],'-dpng','-r200')

%% figure
binMult = -0.005;
fontS = 14;
fontN = 'Cambria';
figW = 5.5;
figH = 9;
tM = 0.12;
lM = 0.55;
rM = 0.12;
bM = .75; 
W = figW-rM-lM;
H = figH-bM-tM;
yL = [1979 2011.2];
yTck = yL(1)+1:yL(2);
xL = [15 32];
xTck = 2:2:xL(2);
tL = [0.005 0];

binX = xL(1):0.4:xL(2);

fig_h = figure('Color','w','Units','inches','Position',[0 0 figW figH],'PaperPosition',[0 0 figW figH]);
movegui(fig_h,'center');

ax_h = axes('Parent',fig_h,'Position',[lM/figW bM/figH W/figW H/figH],...
    'YDir','Reverse','Box','on','LineWidth',1,'TickDir','out',...
    'YLim',yL,'XLim',xL,'YTick',yTck,'XTick',xTck,'FontName',...
    fontN,'FontSize',fontS,'TickLength',tL);

xlabel('Mean July surface temperature (°C)','FontName',fontN,'FontSize',fontS)
hold on;

medianT = zeros(length(years),1);
medVal = medianT;
for yrI = 1:length(years);
    yr = years(yrI);
    useI = eq(yr,year);
    tWtr = wtrS(useI);
    cnt = zeros(length(binX)-1,1);
    for b = 2:length(binX);
        left = binX(b-1);
        right = binX(b);
        cnt(b-1) = sum(ge(tWtr,left) & lt(tWtr,right));
        
    end
    medianT(yrI) = median(tWtr(~isnan(tWtr)));
    cnt(eq(cnt,0)) = NaN;
    plot(binX(1:end-1),cnt*binMult+yr,'k-','LineWidth',1.25,'Parent',ax_h)
    medVal(yrI) = interp1(binX(1:end-1),cnt*binMult+yr,medianT(yrI));
    % do counts here
end

plot(medianT,medVal,'r-','LineWidth',1.5,'Parent',ax_h,'Color',[.7 .2 .2])
print([figDir 'WiLMA_July'],'-dpng','-r200')


end

