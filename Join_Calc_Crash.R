# Calculate crash rates 

# join 2019 crash data with year sum 2019 StreetLight data

# Do same for 2018

# Compare crash rates

# setup ----

source('get_packages.R')

library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)

year = 2019

# Identify the directory which holds the unzipped data files.
data_dir = './data'

if(length(dir(data_dir())) < 1) { stop(paste('Please first obtain the Streetlight Data evaluation data set and unzip into the directory', data_dir))}

if(!dir.exists('Figures')) { dir.create('Figures') }

# Read in the intersection shapefile from one of the files
zones = readOGR(dsn = file.path(data_dir, 'stl_data/128523_Pedestrians_06092020/Shapefile'), layer = '128523_Pedestrians_06092020_zone_activity')

zones = spTransform(zones, CRS("+proj=longlat +datum=WGS84"))

# Pedestrians

ped_file = dir(data_dir)[grep(paste0('Pedestrians_', year, '$'), dir(data_dir))]
ped = read.csv(file.path(data_dir, ped_file, paste0(ped_file, '_za_ped.csv')))

# Bicyclists
bic_file = dir(data_dir)[grep(paste0('Bicyclists_', year, '$'), dir(data_dir))]
bic = read.csv(file.path(data_dir, bic_file, paste0(bic_file, '_za_bike.csv')))

# Vehicles
veh_file = dir(data_dir)[grep(paste0('Vehicle_', year, '$'), dir(data_dir))]
veh = read.csv(file.path(data_dir, veh_file, paste0(veh_file, '_za_all.csv')))

### Filter Based on Aggregates for all day/all day parts
ped = ped %>%
  filter(Day.Part == '0: All Day (12am-12am)',
         Day.Type == '0: All Days (M-Su)') %>%
  mutate(ped_index = Average.Daily.Zone.Traffic..StL.Index.,
         id = Zone.ID) %>%
  select(id, ped_index)

bic = bic %>%
  filter(Day.Part == '0: All Day (12am-12am)',
         Day.Type == '0: All Days (M-Su)') %>%
  mutate(bic_index = Average.Daily.Zone.Traffic..StL.Index.,
         id = Zone.ID) %>%
  select(id, bic_index)

# For testing, we are considering Vehicle **VOLUME** as the index

veh = veh %>%
  filter(Day.Part == '0: All Day (12am-12am)',
         Day.Type == '0: All Days (M-Su)') %>%
  mutate(veh_index = Average.Daily.Zone.Traffic..StL.Volume.,
         id = Zone.ID) %>%
  select(id, veh_index)


ped_bic_veh = left_join(ped, bic)

ped_bic_veh = left_join(ped_bic_veh, veh)

### Read in Crash Data

crashes = read.csv(file.path(data_dir, 'crash', paste0('CRASH_', year, '_PHILADELPHIA.csv')))

# Represent spatially

crashes = crashes %>%
  filter(complete.cases(crashes[c("DEC_LONG", "DEC_LAT")]))

crash_sp = SpatialPointsDataFrame(crashes[c("DEC_LONG", "DEC_LAT")], 
                                 crashes,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

pdf(paste0('Figures/CrashPlotting_', year, '.pdf'))

plot(crash_sp, col = scales::alpha('grey80', 0.5))
plot(zones, add = T)

dev.off(); system('open Figures/CrashPlotting_2019.pdf')

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
         veh_crash_rate = all_crash / mev,
         bic_index_crash_rate = 1000 * bic_crash / bic_index,
         ped_index_crash_rate = 1000 * ped_crash / ped_index
  )


# Plotting crash rate by intersections

ped_crash <- ggplot(crash_dat, 
       aes(x = veh_crash_rate, 
           y = ped_crash_rate,
           size = mev)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) + 
  ylim(0, 0.4) + xlim(0, 0.4) +
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  ylab('Crash Rate - Pedestrian-Involved (crashes per MEV)') +
  xlab('Crash Rate - All Crashes (crashes per MEV)') +
  ggtitle('Philadelphia 2019 Intersection Crash Rates \n based on Streetlight Data volumes')


bic_crash <- ggplot(crash_dat, 
       aes(x = veh_crash_rate, 
           y = bic_crash_rate,
           size = mev)) +
  geom_point(color = scales::alpha('midnightblue', 0.5)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) + 
  ylim(0, 0.4) + xlim(0, 0.4) +
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  ylab('Crash Rate - Bicylcist-Involved (crashes per MEV)') +
  xlab('Crash Rate - All Vehicles (crashes per MEV)') #+
#  ggtitle('Philadelphia 2019 Intersection Crash Rates \n based on Streetlight Data volumes')
ggsave(plot = bic_crash, 
       filename = file.path('Figures','Crash_rates_bic.jpeg'), 
       width = 6, height = 5)


ped_crash <- ggplot(crash_dat, 
                    aes(x = veh_crash_rate, 
                        y = ped_crash_rate,
                        size = mev)) +
  geom_point(color = scales::alpha('midnightblue', 0.5)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) + 
  ylim(0, 0.4) + xlim(0, 0.4) +
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  ylab('Crash Rate - Pedestrian-Involved (crashes per MEV)') +
  xlab('Crash Rate - All Vehicles (crashes per MEV)') #+
#  ggtitle('Philadelphia 2019 Intersection Crash Rates \n based on Streetlight Data volumes')

ggsave(plot = ped_crash, 
       filename = file.path('Figures','Crash_rates_ped.jpeg'), 
       width = 6, height = 5)

library(egg)

gp2 <- ggarrange(bic_crash, ped_crash, ncol = 2,
          padding = unit(1, 'line'),
          labels = c('A. Bicyclist', 'B. Pedestrian'))

ggsave(plot = gp2, 
       filename = file.path('Figures','Crash_rates_bic_ped.jpeg'), 
       width = 8, height = 5)

ped_vs_bic_crash <- ggplot(crash_dat, 
                    aes(x = ped_crash_rate, 
                        y = bic_crash_rate,
                        size = mev,
                        color = veh_crash_rate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) + 
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  scale_color_continuous(name = 'Crash Rate - All Vehicles (crashes per MEV)') +
  ylab('Crash Rate - Bicylcist-Involved (crashes per MEV)') +
  xlab('Crash Rate - Pedestrian-Involved (crashes per MEV)') +
  ggtitle('Philadelphia 2019 Intersection Crash Rates \n based on Streetlight Data volumes')


# Crash rates based on streetlight index ----

ped_hist <- ggplot(crash_dat,
       aes(x = ped_index_crash_rate)) +
  ylab('Count') + xlab('Relative Pedestrian Crash Rate (using Streetlight Index)') + 
  ggtitle('Philadelphia 2019 Intersection Relative Pedestrian Crash Rate') +
  theme_bw() +
  geom_histogram()

bic_hist <- ggplot(crash_dat,
       aes(x = bic_index_crash_rate)) +
  ylab('Count') + xlab('Relative Bicyclist Crash Rate (using Streetlight Index)') + 
  ggtitle('Philadelphia 2019 Intersection Relative Bicyclist Crash Rate') +
  theme_bw() +
  geom_histogram()

ggarrange(ped_hist, bic_hist)

crash_tab <- crash_dat %>%
  filter(bic_crash > 0 &
           ped_crash > 0) %>%
  select(-id, -all_mev, -direction, -is_pass, -is_bidi)

library(DT)
datatable(crash_tab)


## relative risk vs vehicle based crash rate

bc <- ggplot(crash_dat, aes(x = bic_crash_rate, y = bic_index_crash_rate,
                      size = mev)) +
  ylab('Relative bicyclist crash risk (Based on Streetlight Index)') + 
  xlab('Bicyclist Crash Rate (using all vehicle volume)') + 
  ggtitle('Philadelphia 2019 Intersection Relative Bicyclist Crash Rate') +
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  theme_bw() +
  geom_point(color = scales::alpha('midnightblue', 0.5)) 
  

ggsave(plot = bc, 
       filename = file.path('Figures','Crash_rates_bic2.jpeg'), 
       width = 6, height = 5)

pc <- ggplot(crash_dat, aes(x = ped_crash_rate, y = ped_index_crash_rate,
                      size = mev)) +
  ylab('Relative pedestrian crash risk (Based on Streetlight Index)') + 
  xlab('Pedestrian Crash Rate (using all vehicle volume)') + 
  ggtitle('Philadelphia 2019 Intersection Relative Pedestrian Crash Rate') +
  scale_size_continuous(name = 'Million Entering Vehicles \n (MEV)') +
  theme_bw() +
  geom_point(color = scales::alpha('midnightblue', 0.5)) 


ggsave(plot = pc, 
       filename = file.path('Figures','Crash_rates_ped2.jpeg'), 
       width = 6, height = 5)


# Total crashes in intersections ----

hist(crash_dat$all_crash)


hist(crash_dat$bic_crash)


crash_dat %>%
  summarize(sum(all_crash),
            sum(ped_crash),
            sum(bic_crash))
