function [dates,dat,lakeIDs] = wFileOpen(fileName)

dateForm = 'yyyy-mm-dd HH:MM:SS';

% author: Luke A Winslow March 2011
% added: Jordan S Read January 2013
    fid = fopen(fileName);
    

    fgetl(fid);
    lakeHed = fgets(fid);
    splHeds = regexp(lakeHed,'\t','split');
    txEnd   = splHeds{end};
    splHeds{end} = txEnd(1:end-1);
    lakeIDs = cell(length(regexp(lakeHed,'\t','split'))-1,1);
    format = '%s';
    for i=2:length(splHeds);
        format = strcat(format,'%f');
        lakeIDs{i-1} = splHeds{i};
    end
    fgets(fid);
    d = textscan(fid,format,'delimiter','\t','treatAsEmpty',{'na','NA',...
        '#VALUE!','#NAME?'});
    fclose(fid);
    
    
    dat = horzcat(d{2:end});
    dts = regexprep(d{1},{'Z','T'},{'',' '});
    dates = datenum(dts,dateForm);
        

end


