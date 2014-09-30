% Load bathymetric polygons
wgs84 = almanac('earth','wgs84','m');
bathys = shaperead('VectorizedBathymetryFixed.shp','usegeocoords',true);

wbics = [bathys(:).WBIC]';
uWbics = unique(wbics);


for i=1:length(uWbics)
    
    indx = find(uWbics(i) == wbics);
    
    hypso = nan(length(indx),2);
    f = figure();
    ax = subplot(1,2,1);
    ax(2) = subplot(1,2,2);
    hold(ax(1),'all');
    for j=1:length(indx)
        hypso(j,1) = bathys(indx(j)).ContourInt*0.3048;% convert to meters
        
        hypso(j,2) = areaMultipart(bathys(indx(j)).Lat,bathys(indx(j)).Lon,wgs84);    
        
        plot(ax(1),bathys(indx(j)).Lon,bathys(indx(j)).Lat)
        
    end
    
    plot(ax(2),hypso(:,2),hypso(:,1));
    set(ax(2),'ydir','reverse');
    input('blah');
    close(f);
end