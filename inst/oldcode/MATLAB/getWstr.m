function [ Wstr ] = getWstr(hc,lkeArea)

% markfort et al. 2010

hc = max(hc,1);   % must be at least 1 m;
D = 2*sqrt(lkeArea/pi);
Xt = 50*hc; % canopy height times 50

Wstr = 2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2);
if le(D,Xt)
    Wstr = 0.001;
end

end

