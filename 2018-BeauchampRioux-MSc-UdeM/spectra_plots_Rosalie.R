# visualisation spectra data Rosalie Beauchamp-Rioux

library(tidyverse)

# set wd
setwd('/Users/etienne/Documents/ProjetsRecherche/CABO/leafspectra/2018-BeauchampRioux-MSc-UdeM')

# read all leaves from Rosalie project
# average of six leaves (of leaf arrays)
raw_leaves <- read_csv('/Users/etienne/Documents/ProjetsRecherche/leafspectra/2018-BeauchampRioux-MSc-UdeM/data/project_leaves_combined.csv') %>% 
  dplyr::rename(reflectance_transmittance = `reflectance-transmittance`)

# get all (averages)
avg <- read_csv('/Users/etienne/Documents/ProjetsRecherche/leafspectra/2018-BeauchampRioux-MSc-UdeM/data/project_all_combined.csv') %>% 
  select(sample_id, date_measured)
# get only first line per sample_id
# function to get first line
get_first <- function(x) {
  tmp <- x[1,]
  return(tmp)  
}
avg2 <- avg %>% 
  group_by(sample_id) %>%
  do(get_first(.))
 
rm(avg) 

# read plant data
samples <- read_csv('/Users/etienne/Documents/ProjetsRecherche/leafspectra/2018-BeauchampRioux-MSc-UdeM/data/Fulcrum_Export_385fea08-9263-46ba-8ee6-c05f368e363a/bulk_leaf_samples/bulk_leaf_samples.csv') %>% 
  select(sample_id, plant_id)
plants <- read_csv('/Users/etienne/Documents/ProjetsRecherche/leafspectra/2018-BeauchampRioux-MSc-UdeM/data/Fulcrum_Export_385fea08-9263-46ba-8ee6-c05f368e363a/plants/plants.csv') %>% 
  select(plant_id, site_id, scientific_name)

# test with first five records
records <- unique(raw_leaves$sample_id)[1:5]

# merge plants with leaf
raw_leaves_full <- raw_leaves %>% 
  left_join(avg2) %>% 
  left_join(samples) %>% 
  left_join(plants)

rm(raw_leaves)

# subset of first five records
raw_leaves_sub <- raw_leaves %>% 
  filter(sample_id %in% records)

# split into reflectance and transmittance
leaves_refl <- raw_leaves_sub %>% 
  filter(reflectance_transmittance == 'reflectance')

leaves_trans <- raw_leaves_sub %>% 
  filter(reflectance_transmittance == 'transmittance')

#  make pdf plot              
leafplot <- ggplot(leaves_refl, aes(x = wavelength, y = calculated_value)) +
  geom_line() +
  geom_line(aes(y = 1 - calculated_value),
            data = leaves_trans) +
  facet_grid(sample_id ~ leaf_number) +
  ylim(c(0,1))
leafplot

system.time(ggsave('/Users/etienne/Documents/leafplot.pdf', plot = leafplot,
       limitsize = F, width = 12) )

# do it for all leaves
# split into reflectance and transmittance
leaves_refl <- raw_leaves %>% 
  filter(reflectance_transmittance == 'reflectance')

leaves_trans <- raw_leaves %>% 
  filter(reflectance_transmittance == 'transmittance')

#  make pdf plot              
leafplot_full <- ggplot(leaves_refl, aes(x = wavelength, y = calculated_value)) +
  geom_line() +
  geom_line(aes(y = 1 - calculated_value),
            data = leaves_trans) +
  facet_grid(sample_id ~ leaf_number) +
  ylim(c(0,1))
ggsave('/Users/etienne/Documents/leafplot_full.pdf', plot = leafplot_full,
       limitsize = F, width = 12,
       height = 1080)


# create new periods
raw_leaves_full2 <- raw_leaves_full %>%
  mutate(period = date_measured)

raw_leaves_full2$period[raw_leaves_full2$period == '2018-06-08'] <- '2018-06-06'
raw_leaves_full2$period[raw_leaves_full2$period == '2018-06-21'] <- '2018-06-19'
raw_leaves_full2$period[raw_leaves_full2$period == '2018-07-19'] <- '2018-07-17'
raw_leaves_full2$period[raw_leaves_full2$period == '2018-08-16'] <- '2018-08-14'
raw_leaves_full2$period[raw_leaves_full2$period == '2018-09-11'] <- '2018-09-10'
raw_leaves_full2$period[raw_leaves_full2$period == '2018-09-24'] <- '2018-09-23'


#### get mean per species per time for site CF_MSB
temporal_sp_means <- raw_leaves_full2 %>% 
  filter(site_id %in% c('CF_MSB', 'MSB_Parking'),
         date_measured != '2018-07-30') %>% 
  group_by(scientific_name, period, reflectance_transmittance, wavelength) %>% 
  summarise(value_mean = mean(calculated_value, na.rm = T))

temporal_refl <- temporal_sp_means %>% 
  filter(reflectance_transmittance == 'reflectance')
temporal_trans <- temporal_sp_means %>% 
  filter(reflectance_transmittance == 'transmittance') 

### plot it
temporal_plot_msb <- ggplot(temporal_refl, aes(x = wavelength, y = value_mean)) +
  geom_line(aes(colour = as.factor(period) ) ) +
  facet_wrap(~ scientific_name) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)') +
  theme_bw() +
  theme(legend.title = element_blank())
temporal_plot_msb
ggsave('temporal_plot_msb.pdf', width = 8, height = 6)
ggsave('temporal_plot_msb.png', width = 8, height = 6, dpi = 600)

# make a plot of the different species at different sites
env_sp_means <- raw_leaves_full2 %>% 
  filter(!period %in% c('2018-06-06',
                        '2018-09-10',
                        '2018-09-23',
                        '2018-10-08')) %>% 
  group_by(scientific_name, site_id, reflectance_transmittance, wavelength) %>% 
  summarise(value_mean = mean(calculated_value, na.rm = T))


env_refl <- env_sp_means %>% 
  filter(reflectance_transmittance == 'reflectance')
env_trans <- env_sp_means %>% 
  filter(reflectance_transmittance == 'transmittance') 


env_sp_plot <- ggplot(env_refl, aes(x = wavelength, y = value_mean)) +
  geom_line(aes(colour = site_id) ) +
  facet_wrap(~ scientific_name) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)') +
  theme_bw() +
  theme(legend.title = element_blank())
env_sp_plot
ggsave('env_sp_plot.pdf', width = 14, height = 8)
ggsave('env_sp_plot.png', width = 14, height = 8, dpi = 600)

# plot different species at different sites 
env_sp_plot2 <- ggplot(env_refl, aes(x = wavelength, y = value_mean)) +
  geom_line(aes(colour = scientific_name, group = interaction(site_id, scientific_name ) ) ) +
  facet_wrap( ~ site_id) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)') +
  theme_bw() +
  theme(legend.title = element_blank())
env_sp_plot2
ggsave('env_sp_plot2.pdf', width = 14, height = 8)
ggsave('env_sp_plot2.png', width = 14, height = 8, dpi = 600)


# plot all together
env_sp_plot3 <- ggplot(env_refl, aes(x = wavelength, y = value_mean)) +
  geom_line(aes(colour = scientific_name, group = interaction(site_id, scientific_name ) ) ) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)') +
  theme_bw() +
  theme(legend.title = element_blank())
env_sp_plot3
ggsave('env_sp_plot3.pdf', width = 9, height = 5)
ggsave('env_sp_plot3.png', width = 9, height = 5, dpi = 600)  
