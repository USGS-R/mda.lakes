%% Load wisconsin hydrolayer
lShapes = shaperead('WisconsinHydroLayer\wiscoNoZ_wgs84_donut.shp','usegeocoords',true);

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
    
    fpath = ['ManagedLakeDonuts/' num2str(wibcs(i))];
    
    shapewrite(tmp, [fpath '.SHP']);
    copyfile('WisconsinHydroLayer\wiscoNoZ_wgs84_donut.prj',[fpath '.prj']);
end




%% Write just one shape file for all the managed lakes, instead of individuals
lShapes = shaperead('WisconsinHydroLayer\wiscoNoZ_wgs84_donut.shp','usegeocoords',true);

% Extract each of the managed lakes by WBIC
d = importdata('managedLakesWibcs.csv');
wibcs = d.data(:,2);

shapeWibcs = [lShapes(:).WBDY_WBIC]';

[tf,locs] = ismember(wibcs,shapeWibcs);

onlyManagedLakes = lShapes(locs(tf));

shapewrite(onlyManagedLakes,'ManagedLakeShps/managedLakesAllOne.shp');



