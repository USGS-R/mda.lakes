function Kd = getClarity( WBIC,SecchiFile )

% gets the Kd value for a given lake
% WBIC is a string;
if isunix
    del = '/';
else
    del = '\';
end

if eq(nargin,1)
    SecchiFile = ['../supporting files' del 'annual_mean_secchi.txt'];
end
wbic_i = 1;
secc_i = 9;
reader = '%s %s %s %s %s %s %s %s %s %s';

fID = fopen(SecchiFile);
dat = textscan(fID,reader,'Delimiter','\t','HeaderLines',1);
fclose all;

WBICs = dat{wbic_i};
secchis = dat{secc_i};

useI = strcmp(WBICs,WBIC);
if any(useI)
    secchis = str2double(secchis(useI));
    secchis = secchis(~isnan(secchis));
    Secchi = mean(secchis);
else
    Secchi = NaN;
end

Kd = 1.7/(Secchi);


end

