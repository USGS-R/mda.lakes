
for i=1:length(files)

    %Load file
    fromDnr = importdata(['txt_files/' files(i).name],',',1);
    %meHypso = importdata('hypsography.bth');
    %meHypso = meHypso.data;

    vols = fromDnr.data(:,1);
    depths = fromDnr.data(:,2);

    %Cleanup data. Make depths positive down
    %If more than 1/2 of depths are negative, flip
    if(sum(depths < 0) > length(depths)/2)
        depths = depths * -1;
    end

    %Drop any depths above the water surface
    keepIndx = depths > 0;
    vols = vols(keepIndx);
    depths = depths(keepIndx);

    %Now force it through 0,0 (0 volume at 0 depth)
    vols = vertcat(0, vols);
    depths = vertcat(0, depths);

    %Now make data unique on depths
    [depths, uI] = unique(depths);
    vols = vols(uI);

    
    dz = ceil(max(depths)/15);

    newDepths = (0:dz:ceil(max(depths)))';

    newVols = interp1(depths, vols, newDepths)/abs(dz);

    %data are in cumulative volume, differentiate to get
    newAreas = diff(newVols); 


    newDepths = newDepths * 0.3048;  %Convert to meters
    newAreas = newAreas * 4046.85642; %convert acre feet to m^3
    
    
    %Drop the nan from areas
    newAreas = newAreas(1:end-1);
    newDepths = newDepths(1:length(newAreas));
    
    %Make monotonically decreasing.
    [newDepths, sI] = sort(newDepths);
    newAreas = newAreas(sI);
    
    for j = 2:length(newDepths)
        if(newAreas(j) > newAreas(j-1))
            newAreas(j) = newAreas(j-1);
        end
    end
    

    f = figure();
    ax = axes();
    plot(newAreas, newDepths, 'linewidth',2);

    set(ax,'linewidth',2,'tickdir','out')
    set(f,'color','w');
    ylabel('Depth (m)');
    xlabel('Area (m^2)');
    %legend('DNR PDF Derived','From LTER','location','northwest');
    %export_fig hypsoComparison.tiff -r300
    
    %input('blah');
    fid = fopen(['FixedBathys/' files(i).name(1:end-4) '.bth'],'w');
    
    fprintf(fid,'depth\tarea\n');
    
    fprintf(fid,'%g\t%g\n',horzcat(newDepths,newAreas)');
    
    fclose(fid);
    
end
