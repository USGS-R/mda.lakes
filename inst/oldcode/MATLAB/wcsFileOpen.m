function [indx,values]  = wcsFileOpen(fileN)

fID = fopen(fileN);
[~] = fgets(fID); % ID

indx = regexp(fgets(fID),'\t','split');
[~] = fgets(fID); % statID
values = regexp(fgets(fID),'\t','split');
fclose(fID);

indx = indx(2:end);
values = str2double(values(2:end));


end

