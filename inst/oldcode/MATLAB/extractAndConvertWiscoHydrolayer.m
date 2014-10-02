%% Load wisconsin hydrolayer
lShapes = shaperead('WisconsinHydroLayer\wiscoNoZ_wgs84.shp','usegeocoords',true);

%% Extract each of the managed lakes by WBIC
d = importdata('managedLakesWibcs.csv');
wibcs = d.data(:,2);

shapeWibcs = [lShapes(:).WBDY_WBIC]';

[tf,locs] = ismember(wibcs,shapeWibcs);

missing = [];

for i=1:length(tf)
    if(~tf(i))
        missing(length(missing) + 1) = wibcs(i);
        continue;
    end
    
    tmp = lShapes(locs(i));
    
    fpath = ['ManagedLakeShps/' num2str(wibcs(i))];
    
    shapewrite(tmp, [fpath '.SHP']);
    copyfile('WisconsinHydroLayer\wiscoNoZ_wgs84.prj',[fpath '.prj']);
end



