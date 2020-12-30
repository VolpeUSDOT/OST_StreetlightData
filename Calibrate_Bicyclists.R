# Figure out what calibration data we might have available

# setup ----

library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)

year = 2019

data_dir = './data'

zones = readOGR(dsn = file.path(data_dir, 'stl_data/128523_Pedestrians_06092020/Shapefile'), layer = '128523_Pedestrians_06092020_zone_activity')

zones = spTransform(zones, CRS("+proj=longlat +datum=WGS84"))

# Bicyclists
bic_file = dir(data_dir)[grep(paste0('Bicyclists_', year, '$'), dir(data_dir))]
bic = read.csv(file.path(data_dir, bic_file, paste0(bic_file, '_za_bike.csv')))

### Filter Based on Aggregates for all day/all day parts
bic = bic %>%
  filter(Day.Part == '0: All Day (12am-12am)',
         Day.Type == '0: All Days (M-Su)') %>%
  mutate(bic_index = Average.Daily.Zone.Traffic..StL.Index.,
         id = Zone.ID) %>%
  select(id, bic_index)

### Read in DVRPC bicycle count data


counts = readOGR(dsn = file.path(data_dir, 'Bike Counts DVRPC'), layer = 'DVRPC_Bicycle_Counts')

# Represent spatially

pdf(paste0('Count_2018_2019.pdf'))

plot(zones, col = scales::alpha('grey20', alpha = 0.8),
     main = 'Streetlight Zones and DVRPC counter data')
plot(counts[counts$setyear == '2019',], 
     pch = 21, 
     bg = scales::alpha('white', 0.1),
     col = scales::alpha('blue', 0.8),
     add = T)

plot(counts[counts$setyear == '2018',], 
     pch = 21, 
     bg = scales::alpha('white', 0.1),
     col = scales::alpha('red', 0.8),
     add = T)

dev.off()

# Add crashes to zones


stopifnot(all(zones@data$id %in% ped_bic_veh$id) & all(ped_bic_veh$id %in% zones@data$id))

zone_data = left_join(zones@data, ped_bic_veh)

zones@data = zone_data

head(zones@data)

crash_z <- over(crash_sp, zones["id"])

crash_sp@data$id = crash_z

crash_in_zone = crash_sp[!is.na(crash_z[,'id']),]

crash_in_zone$id = crash_z[!is.na(crash_z[,'id']),]

crash_agg = crash_in_zone@data %>%
  group_by(id) %>%
  summarize(all_crash = n(),
            bic_crash = sum(BICYCLE_COUNT > 0),
            ped_crash = sum(PED_COUNT > 0))

zones_data = left_join(zones@data, crash_agg, by = 'id')
zones_data[is.na(zones_data)] = 0

zones@data = zones_data

writeOGR(zones,
         dsn = data_dir,
         layer = paste0('StL_Joined_', year),
         driver = 'ESRI Shapefile')

# Crash rate calcs ----

# Calculate mev to have scaled traffic value https://safety.fhwa.dot.gov/local_rural/training/fhwasa14072/sec4.cfm
# Here for a single year; original analysis was over 11 years

crash_dat = zones@data

crash_dat = crash_dat %>%
  ungroup() %>%
  mutate(ped_index = as.numeric(ped_index),
         bic_index = as.numeric(bic_index),
         veh_index = as.numeric(veh_index)) %>%
  mutate(all_mev = ((ped_index + bic_index + veh_index) * 365 * 1)/1000000,
         #bic_mev = ((veh_index) * 365 * 1)/1000000,
         mev = (veh_index  * 365 * 1)/1000000,
         ped_crash_rate = ped_crash / mev,
         bic_crash_rate = bic_crash / mev,
         veh_crash_rate = all_crash / mev
  )





