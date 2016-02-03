# Process the raw WiDNR Database output. Mostly just metadata and units cleanup

Sys.setenv(tz='GMT')

d = read.csv('data-raw/WiDNR/temp_DO.csv', header=TRUE, as.is=TRUE)

d$date = as.POSIXct(d$START_DATETIME)
d$Dissolved.Oxygen.Units = tolower(d$Dissolved.Oxygen.Units)
d$UNIT_CODE = tolower(d$UNIT_CODE)
d$UNIT_CODE_1 = tolower(d$UNIT_CODE_1)
#set empty DO to NA
d$Dissolved.Oxygen[d$Dissolved.Oxygen==''] = NA
d$Dissolved.Oxygen = as.numeric(d$Dissolved.Oxygen)

#just want DO data as mg/l or ppm (same thing)
d = subset(d, Dissolved.Oxygen.Units == 'mg/l' || Dissolved.Oxygen.Units == 'ppm')

#merge START_AMT and START_AMT_1
missing_start = is.na(d$START_AMT)
d$START_AMT[missing_start] = d$START_AMT_1[missing_start]
d$UNIT_CODE[missing_start] = d$UNIT_CODE_1[missing_start]

d$UNIT_CODE_1 = NULL
d$START_AMT_1 = NULL

#convert UNIT_CODE from FEET/FT to METERS/M (there are inches in there, but I don't trust them)

old_units = d$UNIT_CODE == 'feet'
d$START_AMT[old_units] = d$START_AMT[old_units]* 0.3048
d$UNIT_CODE[old_units] = 'meters'

#drop the weird waterbody types
d = subset(d, Waterbody.Type != 'RIVER')
d = subset(d, Waterbody.Type != 'GRAVEL-PIT')



#cleanup header
tosave = d[,c('WBIC', 'date', 'START_AMT', 'Dissolved.Oxygen')]
names(tosave) = c('WBIC', 'date', 'depth', 'doobs_mg_l')
tosave = na.omit(tosave)

#drop impossibly  high (and negative) DO values
tosave = tosave[tosave$doobs_mg_l < 20 & tosave$doobs_mg_l >= 0, ]

write.table(tosave, 'inst/supporting_files/doobs.obs.tsv', sep='\t', row.names=FALSE)

