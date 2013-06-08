function writeNMLs4GLM



refreshWiLMAfiles()

lakeIDs = getLakeIDs();

for j = 1:length(lakeIDs)
    lakeID = lakeIDs{j};
    Kd = getClarity(lakeID);
    bth= getBathy(lakeID);
    if isnan(Kd)
        Kd = 1;
    end
    
    elev = getElev(lakeID);
    canopy = getCanopy(lakeID);
    lkeArea = getArea(lakeID);
    [lat,long] = getLatLong(lakeID);    % Luke?
    Wstr = getWstr(canopy,lkeArea);
    
    Cu   = Wstr^0.33333;
    bthH = bth(1,:);
    bthA = bth(2,:);
    for i = 1:length(bth(1,:))
        bthH(i) = elev-bth(1,length(bthH)-i+1);
        bthA(i) = bth(2,length(bthA)-i+1)/1000; 
    end
    lakeRef = ['WBIC_' lakeID];
    metFile = ['WBIC_' lakeID '.csv'];
    
    writeGLMnmlParamFile('Kw_FLT',Kd,'lake_name_STR',lakeRef,...
        'latitude_FLT',lat,'longitude_FLT',long,...
        'H_csvVEC',bthH,'A_csvVEC',bthA,'meteo_fl_STR',metFile,...
        'wind_factor_FLT',1,'ce_FLT',0.0013/Cu,'ch_FLT',0.0013/Cu,...
        'stop_STR','2011-12-31 23:00:00','min_layer_thick_FLT',0.1,...
        'max_layer_thick_FLT',1)
    toc
    disp(['lake ' num2str(lk) ' of ' num2str(length(lakeIDs))]);
    disp('-----');
    %     else
    
end
end

