function Kd = getClarity( WBIC )

% gets the Kd value for a given lake
% WBIC is a string;



SecchiFile = ['/Users/jread/Desktop/Science Projects/'...
    'WiLMA/GLM files/Metadata/annual_mean_secchi.txt'];
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

