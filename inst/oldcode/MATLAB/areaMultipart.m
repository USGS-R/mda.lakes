function area = areaMultipart(lats, lons, ellipsoid)
%AREALAKE Returns area of lake polygon.
%Works almost exactly like AREAINT, only properly subtracts the area of
%islands from the overall area.

if(nargin < 3)
    ellipsoid = [1 0];
end

if(~isShapeMultipart(lats,lons))
    area = areaint(lats,lons,ellipsoid);
else
    [splitlats,splitlons] = polysplit(lats,lons);
    outer = ~ispolycw(splitlats,splitlons);
    [outlat,outlon] = polyjoin(splitlats(outer),splitlons(outer));
    [inlat,inlon] = polyjoin(splitlats(~outer),splitlons(~outer));
    
    %We may have multipart objects that don't have inner parts, just
    %multiple outer parts (useful for overlapping area detection)
    if(isempty(inlat))
        area = sum(areaint(outlat,outlon,ellipsoid));
    else
        area = sum(areaint(outlat,outlon,ellipsoid)) - ...
            sum(areaint(inlat,inlon,ellipsoid));
    end
end

end