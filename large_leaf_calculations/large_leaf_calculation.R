# test large leaf calculation

library(tidyverse)

# using record 10277030 to test calculations (no real reason why that one)
record_no <- 10277030

leaves <- read_csv('http://data.caboscience.org/field-data/projects/2018-BeauchampRioux-MSc-UdeM/spectra/processed/2018-06-19-CF_MSB-2092/10277030/leaves.csv')

# using the data share
spectra_fulcrum <- read_csv('https://web.fulcrumapp.com/shares/5c5290b884ab3734.csv')
spectra_child_fulcrum <- read_csv('https://web.fulcrumapp.com/shares/5c5290b884ab3734.csv?child=measurements')

# keep only fulcrum data for selected record
spectra_sub <- filter(spectra_fulcrum, sample_id == record_no)
child_sub <- inner_join(spectra_child_fulcrum, spectra_sub, by = c('fulcrum_parent_id' = 'fulcrum_id'))

# panel
panel <- select(spectra_sub, panel_id)
panel_id <- panel$panel_id
panel_fulcrum <- read_csv('https://web.fulcrumapp.com/shares/c3c9cb2d8b913339.csv')
panel_calibs <- read_csv('https://web.fulcrumapp.com/shares/c3c9cb2d8b913339.csv?child=calibrations') %>% 
  filter(fulcrum_parent_id == panel_id)
panel_calibs_id <- panel_calibs$fulcrum_id
panel_refls <- read_csv('https://web.fulcrumapp.com/shares/c3c9cb2d8b913339.csv?child=calibrated_reflectance') %>% 
  filter(fulcrum_parent_id == panel_calibs_id) %>% 
  select(wavelength_nm, reflectance) %>% 
  rename(wvl = wavelength_nm, refl_panel = reflectance) 
str(panel_refls)
panel_plot <- ggplot(panel_refls, aes(x = wvl, y = refl_panel)) +
  geom_line()
panel_plot
ggsave('panel_99AA02-0318-9712.png', width = 6, height = 4.5)


##### Reflectance ----

# A: Reflectance, leaf, reference (R_ref)
R_ref <- child_sub %>% 
  filter(sphere_configuration_svc_large_leaves == 'A: Reflectance')
R_ref_file <- R_ref$file_name[1] # first reference

# B: Reflectance, stray light (R_str)
R_str <- child_sub %>% 
  filter(sphere_configuration_svc_large_leaves == 'B: Reflectance') %>% 
  select(file_name)
R_str_file <- R_str$file_name

# C: Reflectance, leaf, target (R_tar_a)
R_tar <- child_sub %>% 
  filter(sphere_configuration_svc_large_leaves == 'C: Reflectance')
R_tar_files <- R_tar$file_name # first reference

#### get data together

# url prefix
pref <- c('http://data.caboscience.org/field-data/projects/2018-BeauchampRioux-MSc-UdeM/spectra/processed/2018-06-19-CF_MSB-2092/10277030/interpolated_files/')

# leaf  1
R_tar_1_path <- paste0(pref, R_tar_files[1], '.sig.txt')
R_tar_1_raw <- read.table(R_tar_1_path, skip = 19) %>% 
  rename(wvl = V1, R_tar = V2)

# ref
R_ref_path <- paste0(pref, R_ref_file, '.sig.txt')
R_ref_raw <- read.table(R_ref_path, skip = 19) %>% 
  rename(wvl = V1, R_ref = V2)

# stray
R_str_path <- paste0(pref, R_str_file, '.sig.txt')
R_str_raw <- read.table(R_str_path, skip = 19) %>% 
  rename(wvl = V1, R_str = V2)

# put together for leaf array 1
leaf1_df <- R_tar_1_raw %>% 
  mutate(R_ref = R_ref_raw$R_ref,
         R_str = R_str_raw$R_str) %>% 
  mutate(wvl_num = as.numeric(as.character(wvl) ) ) %>% 
  filter(wvl_num <= 2513, wvl_num >= 338) %>% 
  left_join(panel_refls, by = c('wvl_num' = 'wvl') )%>% 
  mutate(leaf = 1,
         R_tar = as.numeric(as.character(R_tar)),
         R_ref = as.numeric(as.character(R_ref)),
         R_str = as.numeric(as.character(R_str))) %>% 
  filter(!is.na(refl_panel))

# get corrected reflectance from jeremy's files
leaf1_jer <- leaves %>% 
  select(wavelength, calculated_value, leaf_number, `reflectance-transmittance`) %>%
  rename(wvl_num = wavelength, refl_leaf_jeremy = calculated_value, reflectance_transmittance = `reflectance-transmittance` ) %>% 
  filter(leaf_number == 1, reflectance_transmittance == 'reflectance',
         wvl_num <= 2500)

# calculate reflectance of leaf  1
leaf1 <- leaf1_df %>% 
  mutate(refl_leaf_etienne = ((R_tar - R_str) / (R_ref - R_str) ) * refl_panel,
         refl_leaf_jeremy = leaf1_jer$refl_leaf_jeremy)


# gather
leaf_long <- leaf1 %>% 
  select(wvl_num, refl_panel, refl_leaf_etienne, refl_leaf_jeremy) %>% 
  rename(wvl = wvl_num,
         panel = refl_panel,
         leaf_corr_etienne = refl_leaf_etienne,
         leaf_corr_jeremy = refl_leaf_jeremy) %>% 
  gather(key = target, value = refl, -wvl)

# make plot
refl_plot <- ggplot(leaf_long, aes(x = wvl, y = refl)) +
  geom_line(aes(colour = target)) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)')
refl_plot
ggsave('refl_plot.pdf', width = 6, height = 4.5)
