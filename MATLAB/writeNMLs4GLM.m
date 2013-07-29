function writeNMLs4GLM(writeRoot,lakeIDs)

refresh = false;
if refresh
    refreshWiLMAfiles()
end

if eq(nargin,0)
    writeRoot = 'D:\WiLMA\NML\';
    fID = fopen('D:\WiLMA\to_cal_wbic.csv');
    lakeIDs = textscan(fID,'%s','HeaderLines',1,'Delimiter',',');
    fclose(fID);
    lakeIDs = lakeIDs{1};
elseif eq(nargin,1)
    fID = fopen('D:\WiLMA\to_cal_wbic.csv');
    lakeIDs = textscan(fID,'%s','HeaderLines',1,'Delimiter',',');
    fclose(fID);
end





for j = 1:length(lakeIDs)
    lakeID = lakeIDs{j};
    Kd = getClarity(lakeID);
    bth= getBathy(lakeID);
    if isnan(Kd)
        Kd = 0.63; % overall mean
    end
    
    elev = getElev(lakeID);
    metaFile = '../supporting files/canopyht_zonal_no_zero_num5_positivehts.csv';
    canopy = getCanopy(lakeID,metaFile);
    lkeArea = getArea(lakeID);
    [lat,long] = getLatLon(lakeID);
    Wstr = getWstr(canopy,lkeArea);
    
    Cu   = Wstr^0.33333;
    bthH = bth(1,:);
    
    
    bthA = bth(2,:);
    for i = 1:length(bth(1,:))
        bthH(i) = elev-bth(1,length(bthH)-i+1);
        bthA(i) = bth(2,length(bthA)-i+1)/1000; 
    end
    
    if ge(max(bthH),20)
        mxLyr = 1.5;
    elseif ge(max(bthH),8) && lt(max(bthH),20)
        mxLyr = 1.0;
    elseif ge(max(bthH),5) && lt(max(bthH),8)
        mxLyr = .8;
    elseif ge(max(bthH),3) && lt(max(bthH),5)
        mxLyr = .5;
    else
        mxLyr = .3;
    end
    
    lakeRef = ['WBIC_' lakeID];
    metFile = ['WBIC_' lakeID '.csv'];
    bsn_len = sqrt((max (bthA)*1000)/pi())*2;
    bsn_wid = sqrt((max(bthA)*1000)/pi())*2;
    
    simDir = [writeRoot lakeRef '/']; 
    mkdir(simDir);
    
    writeGLMnmlParamFile(simDir,'Kw_FLT',Kd,'lake_name_STR',lakeRef,...
        'latitude_FLT',lat,'longitude_FLT',long,...
        'H_csvVEC',bthH,'A_csvVEC',bthA,'meteo_fl_STR',metFile,...
        'ce_FLT',0.0014,'ch_FLT',0.0014,...
        'stop_STR','2011-12-31 23:00:00','min_layer_thick_FLT',0.1,...
        'max_layer_thick_FLT',mxLyr,'dt_FLT',86400,'nsave_INT',1,...
        'coef_wind_drag_FLT',0.0016*Cu,...
        'bsn_len_FLT',bsn_len,'bsn_wid_FLT',bsn_wid);
    
    disp(['lake ' num2str(j) ' of ' num2str(length(lakeIDs))]);
    disp('-----');
    %     else
    
end
end

