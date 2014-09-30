function [fixedStruc] = fixAndCombinePolygon(polyStrucs)

%FIRST, we need to do a stupid thing, check if any lines need to be
%combined to make a polygon. 
% for i=1:length(polyStrucs)
%     
%     %Assume that if one point is in, all points are in
%     toCheck = [polyStrucs(i).Lon(1) polyStrucs(i).Lat(1)];
%     
%     for j=1:length(polyStrucs)
%         if(j==i || isInsideAnother(i)) %don't match self and don't check again if already inside some polygon
%             continue;
%         end
%         
%         in = inpolygon(toCheck(1), toCheck(2), polyStrucs(j).Lon, polyStrucs(j).Lat);
%         
%         isInsideAnother(i) = isInsideAnother(i) || in;
%     end
% end
% 

%Second, we need to find all polygons which are not inside another polygon
isInsideAnother = false(size(polyStrucs));


for i=1:length(polyStrucs)
    
    %Assume that if one point is in, all points are in
    toCheck = [polyStrucs(i).Lon(1) polyStrucs(i).Lat(1)];
    
    for j=1:length(polyStrucs)
        if(j==i || isInsideAnother(i)) %don't match self and don't check again if already inside some polygon
            continue;
        end
        
        in = inpolygon(toCheck(1), toCheck(2), polyStrucs(j).Lon, polyStrucs(j).Lat);
        
        isInsideAnother(i) = isInsideAnother(i) || in;
    end
end


iOut = find(~isInsideAnother,1,'first');
fixedStruc = polyStrucs(iOut);

[fixedStruc.Lon fixedStruc.Lat] = poly2cw(fixedStruc.Lon, fixedStruc.Lat);

for i=1:length(polyStrucs)
    if(i == iOut)%Skip the one we started with
        continue; 
    end
    
    if(isInsideAnother(i))
        [lon,lat] = poly2ccw(polyStrucs(i).Lon, polyStrucs(i).Lat);
    else
        [lon,lat] = poly2cw(polyStrucs(i).Lon, polyStrucs(i).Lat);
    end
    
    if(~isnan(fixedStruc.Lon(end)))
        fixedStruc.Lon(end+1) = NaN;
        fixedStruc.Lat(end+1) = NaN;
    end
    
    fixedStruc.Lon = horzcat(fixedStruc.Lon, lon);
    fixedStruc.Lat = horzcat(fixedStruc.Lat, lat);
    
end










end