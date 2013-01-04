%% Ok, lets extract some information from these bathymetry data

% I previously projected all the data to wgs84 and saved as *.shp file
bathys = shaperead('VectorizedBathymetry.shp','usegeocoords',true);

%we'll organize these by wbic numbers
%bathys = bathys(find(strcmpi({bathys.Lake_Name},'Lake Mendota')));
wbics = [bathys(:).WBIC]';
uWbics = unique(wbics);
depths = [bathys(:).ContourInt]';

%Now, let's "do something" (hypsography for example) for each wbic

fixedPolys = bathys(1);%seed it with this

for i=1:length(uWbics)

    indx = wbics == uWbics(i);
    uDepths = unique(depths(indx));
    
    for j=1:length(uDepths)
        
        tmp = bathys(indx & uDepths(j) == depths);
        %%Lake specific fixes
        if(i== 81 && j == 2)
            tmp(1).Lon = horzcat(tmp(1).Lon(1:end-1),fliplr(tmp(2).Lon(1:end-1)),NaN);tmp(1).Lat = horzcat(tmp(1).Lat(1:end-1),fliplr(tmp(2).Lat(1:end-1)),NaN);
            tmp(2) = [];
        end
        
        if(i == 82 && j == 2)
            tmp(2).Lon = horzcat(tmp(2).Lon(1:end-1),tmp(3).Lon); tmp(2).Lat = horzcat(tmp(2).Lat(1:end-1),tmp(3).Lat);
            tmp(3) = [];
        end
        
        fixedPolys(length(fixedPolys)+1) = fixAndCombinePolygon(tmp);
        
        %figure();
        
        %[f,v] = poly2fv(fixedPolys(end).Lon,fixedPolys(end).Lat);
        %patch('Faces', f, 'Vertices', v, 'FaceColor', 'r', ...
        %      'EdgeColor', 'none');
        %input('blah');
    end
    
end

%Strip the seed first val
fixedPolys = fixedPolys(2:end);


for i=1:length(fixedPolys)
    [latc,lonc] = polysplit(fixedPolys(i).Lat,fixedPolys(i).Lon);
    
    for j=1:length(latc)
        if(ispolycw(lonc{j},latc{j}))
            plot(lonc{j},latc{j},'b');
        else
            plot(lonc{j},latc{j},'r');
        end
        hold on;
    end
    input('blah');
end

%shapewrite(fixedPolys,'VectorizedBathymetryFixed.shp');