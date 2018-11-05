# visualisation spectra data Rosalie Beauchamp-Rioux

library(tidyverse)

# read all leaves from Rosalie project
# average of six leaves (of leaf arrays)
raw_leaves <- read_csv('data/project_leaves_combined.csv')

# split into reflectance and transmittance
refl <- raw_leaves %>% 
  filter(reflectance_transmittance == 'reflectance')

trans <- raw_leaves %>% 
  filter(reflectance_transmittance == 'transmittance')
     

#                
leafplot <- ggplot(refl, aes(x = wavelength, y = calculated_value)) +
  geom_line() +
  geom_line(aes(y = 1 - calculated_value),
            data = leaves_trans) +
  facet_grid(sample_id ~ leaf_number, ncol = 6) +
  ylim(c(0,1))