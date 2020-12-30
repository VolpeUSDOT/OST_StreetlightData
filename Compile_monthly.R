# Compile the monthly Streetlight Data index data from the data extracted from the Insight platform

# Setup ----

source('get_packages.R')

library(tidyverse)

# Identify the directory which holds the unzipped data files.
data_dir = './data'

if(length(dir(data_dir())) < 1) { stop(paste('Please first obtain the Streetlight Data evaluation data set and unzip into the directory', data_dir))}

if(!dir.exists('Figures')) { dir.create('Figures') }

monthly_dirs = dir(data_dir)[grep('Bicyclists_20\\d{2}_\\d{2}', dir(data_dir))]

# From za zonal attributes data files within month
# Trip count: Average Daily Zone Traffic (StL Index)
# Trip Duration: Avg Trip Duration (sec)
# Trip length: Avg Trip Length (mi)

compiled = vector()

for(d in monthly_dirs){
  # d = monthly_dirs[31]
  dirx <- dir(file.path(data_dir, d))
  za_file = dirx[grep('_za_bike.csv', dirx)]
  dx <- read.csv(file.path(data_dir, d, za_file))
  
  Yearx = strsplit(d, '_')[[1]][3]
  Monthx = strsplit(d, '_')[[1]][4]
  
  d_select = dx %>%
    filter(Day.Type == '0: All Days (M-Su)',
           Day.Part == '0: All Day (12am-12am)') %>%
    rename(StL_Index = Average.Daily.Zone.Traffic..StL.Index.,
           Trip_dur = Avg.Trip.Duration..sec.,
           Trip_len = Avg.Trip.Length..mi.) %>%
    mutate(Year = Yearx,
           Month = Monthx) %>%
    select(Year, Month, Zone.ID, Zone.Name, 
           StL_Index, Trip_dur, Trip_len)
    
  
  compiled = rbind(compiled, d_select)
  
}


write.csv(compiled, file = file.path(data_dir, 'Compiled_Monthly_StL.csv'), row.names = F)

# plotting ----

d <- compiled %>%
  mutate(ym = as.POSIXct(paste(Year, Month, '01', sep = '-'), format = '%Y-%m-%d'))

d <- d[order(d$Year, d$Month, d$Zone.ID),]

d_mo = d %>%
  group_by(ym) %>%
  summarize(mean_StL = mean(StL_Index),
            sd_StL = sd(StL_Index),
            mean_dur = mean(Trip_dur/60),
            sd_dur = sd(Trip_dur/60),
            mean_len = mean(Trip_len),
            sd_len = sd(Trip_len)
            )

index_plot <- ggplot(d_mo, 
       aes(x = ym,
           y = mean_StL)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = mean_StL - sd_StL,
                  ymax = mean_StL + sd_StL),
              alpha = 0.1) +
  xlab('Date') + ylab('Streetlight Index \n (mean +/- 1 s.d.)') +
  ggtitle('Bicyclist Streetlight Index count (uncalibrated) across 50 intersections, \n 2018-2020')

index_plot

ggsave('Figures/Mean_Streetlight_Index.jpeg', width = 8, height = 5)


index_plot_b <- ggplot(d, 
                     aes(x = ym,
                         y = StL_Index,
                         color = as.factor(Zone.ID))) +
  #geom_point() +
  geom_line() +
  theme_bw() +
  xlab('Date') + ylab('Streetlight Index \n (mean +/- 1 s.d.)') +
  ggtitle('Bicyclist Streetlight Index count (uncalibrated) for 50 intersections, \n 2018-2020')

index_plot_b

ggsave('Figures/By_Intersection_Streetlight_Index.jpeg', width = 8, height = 5)

dur_plot <- ggplot(d_mo, 
                     aes(x = ym,
                         y = mean_dur)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = mean_dur - sd_dur,
                  ymax = mean_dur + sd_dur),
              alpha = 0.1) +
  xlab('Date') + ylab('Streetlight Trip Duration \n (mean +/- 1 s.d.)') +
  ggtitle('Bicyclist Streetlight Trip Duration (min) across 50 intersections, \n 2018-2020')

dur_plot

ggsave('Figures/Mean_Streetlight_Duration.jpeg', width = 8, height = 5)

dist_plot <- ggplot(d_mo, 
                   aes(x = as.Date(ym),
                       y = mean_len)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = mean_len - sd_len,
                  ymax = mean_len + sd_len),
              alpha = 0.1) +
  xlab('Date') + ylab('Streetlight Trip Length \n (mean +/- 1 s.d.)') +
  scale_x_date(date_breaks = "6 months" , date_labels = "%b-%y") +
  ggtitle('Bicyclist Streetlight Trip Length (miles) across 50 intersections, \n 2019-2020')

dist_plot

ggsave('Figures/Mean_Streetlight_Length.jpeg', width = 8, height = 5)

combo <- egg::ggarrange(index_plot,
               dur_plot,
               dist_plot,
               nrow = 3)

ggsave(plot = combo,
       filename = 'Figures/Combined_Streetlight_Plots.jpeg', width = 11, height = 8)


# Percent change from 2019 average, by station --
# COVID impacts

View(d)
