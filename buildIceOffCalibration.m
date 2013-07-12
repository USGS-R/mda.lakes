function buildIceOffCalibration

% writes to a file that I will use R to read and apply lm() to.

% do any of these lake years have ice-off that occurs before Jan 1st???

% vars needed:
% - ice off date (julian day)
% - daily air temperature for that year
% - sun angle (requires lat and long)
% - surface area of lake
% - longitude of lake
% - elevation of lake

%% open ice cover file and extract necessary information
fID = fopen('supporting files/Validation/ice_data.csv');

dat = textscan(fID,'%f %f %f %f %f %f %s %s %f %f %s %s','Delimiter',',',...
    'HeaderLines',1);

fclose(fID);

dat



end

