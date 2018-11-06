# test small leaf calculation

library(tidyverse)

# using record 11926576 to test calculations (no real reason why that one)
record_no <- 11926576

leaves <- read_csv('http://data.caboscience.org/field-data/projects/2018-Girard-MSc-UdeM/spectra/processed/2018-07-08-MBP_open-2093/11926576/leaves.csv')

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
ggsave('/Users/etienne/Documents/panel_99AA02-0318-9711.png', width = 6, height = 4.5)

##### Reflectance ----

# A: Reflectance, leaf array, reference (R_ref_a) for leaf arrays 1-3
R_ref_a_13 <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'A: Reflectance')
R_ref_a_13_file <- R_ref_a_13$file_name[1] # first reference

# A: Reflectance, leaf array, reference (R_ref_a) for leaf arrays 4-6
R_ref_a_46 <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'A: Reflectance')
R_ref_a_46_file <- R_ref_a_13$file_name[2] # second reference

# B: Reflectance, leaf array + paper, reference (R_ref_ap) for leaf arrays 1-3
R_ref_ap_13 <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'B: Reflectance')
R_ref_ap_13_file <- R_ref_ap_13$file_name[1] # first reference

# B: Reflectance, leaf array + paper, reference (R_ref_ap) for leaf arrays 4-6
R_ref_ap_46 <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'B: Reflectance')
R_ref_ap_46_file <- R_ref_ap_46$file_name[2] # first reference

# C: Reflectance, paper, reference (R_ref_p)
R_ref_p <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'C: Reflectance') %>% 
  select(file_name)
R_ref_p_file <- R_ref_p$file_name

# D: Reflectance, stray light (R_str)
R_str <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'D: Reflectance') %>% 
  select(file_name)
R_str_file <- R_str$file_name

# E: Reflectance, paper, target (R_tar_p)
R_tar_p <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'E: Reflectance') %>% 
  select(file_name)
R_tar_p_file <- R_tar_p$file_name

# F: Reflectance, leaf array #1-6 + paper, target (R_tar_ap)
R_tar_ap <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'F: Reflectance')
R_tar_ap_files <- R_tar_ap$file_name

# G: Reflectance, leaf array #1-6, target (R_tar_a)
R_tar_a <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'G: Reflectance')
R_tar_a_files <- R_tar_a$file_name


#### get data together

# url prefix
pref <- c('http://data.caboscience.org/field-data/projects/2018-Girard-MSc-UdeM/spectra/processed/2018-07-08-MBP_open-2093/11926576/interpolated_files/')

# leaf array 1 + paper
R_tar_ap_1_path <- paste0(pref, R_tar_ap_files[1], '.sig.txt')
R_tar_ap_1_raw <- read.table(R_tar_ap_1_path, skip = 19) %>% 
  rename(wvl = V1, R_tar_ap = V2) 

# leaf array 1
R_tar_a_1_path <- paste0(pref, R_tar_a_files[1], '.sig.txt')
R_tar_a_1_raw <- read.table(R_tar_a_1_path, skip = 19) %>% 
  rename(wvl = V1, R_tar_a = V2)

# ref, leaf array 1-3
R_ref_a_13_path <- paste0(pref, R_ref_a_13_file, '.sig.txt')
R_ref_a_13_raw <- read.table(R_ref_a_13_path, skip = 19) %>% 
  rename(wvl = V1, R_ref_a = V2) 

# ref, leaf array + paper 1-3
R_ref_ap_13_path <- paste0(pref, R_ref_ap_13_file, '.sig.txt')
R_ref_ap_13_raw <- read.table(R_ref_ap_13_path, skip = 19) %>% 
  rename(wvl = V1, R_ref_ap = V2)

# stray
R_str_path <- paste0(pref, R_str_file, '.sig.txt')
R_str_raw <- read.table(R_str_path, skip = 19) %>% 
  rename(wvl = V1, R_str = V2)

# ref, paper
R_ref_p_path <- paste0(pref, R_ref_p_file, '.sig.txt')
R_ref_p_raw <- read.table(R_ref_p_path, skip = 19) %>% 
  rename(wvl = V1, R_ref_p = V2)

# target, paper
R_tar_p_path <- paste0(pref, R_tar_p_file, '.sig.txt')
R_tar_p_raw <- read.table(R_tar_p_path, skip = 19) %>% 
  rename(wvl = V1, R_tar_p = V2)

# put together for leaf array 1
array1_df <- R_tar_ap_1_raw %>% 
  mutate(R_tar_a = R_tar_a_1_raw$R_tar_a,
         R_ref_a = R_ref_a_13_raw$R_ref_a,
         R_str = R_str_raw$R_str,
         R_ref_p = R_ref_p_raw$R_ref_p,
         R_ref_ap = R_ref_ap_13_raw$R_ref_ap,
         R_tar_p = R_tar_p_raw$R_tar_p) %>% 
  mutate(wvl_num = as.numeric(as.character(wvl) ) ) %>% 
  filter(wvl_num <= 2513, wvl_num >= 338) %>% 
  left_join(panel_refls, by = c('wvl_num' = 'wvl') )%>% 
  mutate(array = 1,
         R_tar_ap = as.numeric(as.character(R_tar_ap)),
         R_tar_a = as.numeric(as.character(R_tar_a)),
         R_ref_a = as.numeric(as.character(R_ref_a)),
         R_ref_ap = as.numeric(as.character(R_ref_ap)),
         R_str = as.numeric(as.character(R_str)),
         R_ref_p = as.numeric(as.character(R_ref_p)),
         R_tar_p = as.numeric(as.character(R_tar_p)),
         R_ref_ap = as.numeric(as.character(R_ref_ap)) ) %>% 
  filter(!is.na(refl_panel)) %>% 
  mutate(refl_p = ((R_tar_p - R_str) / (R_ref_p - R_str) ) * refl_panel)


# calculate gap fraction
gap <- array1_df %>% 
  filter(wvl_num == 400) %>% 
  mutate(gap_frac = (((R_tar_ap - R_str) / (R_ref_ap - R_str) ) - ((R_tar_a - R_str)/ (R_ref_a - R_str) ) ) * (refl_panel / refl_p)   )
G <- gap$gap_frac

# get corrected reflectance from jeremy's files
array1_jer <- leaves %>% 
  select(wavelength, calculated_value, leaf_number, `reflectance-transmittance`) %>%
  rename(wvl_num = wavelength, refl_array_jeremy = calculated_value, reflectance_transmittance = `reflectance-transmittance` ) %>% 
  filter(leaf_number == 1, reflectance_transmittance == 'reflectance',
         wvl_num <= 2500)

# calculate reflectance of leaf array 1
array1 <- array1_df %>% 
  mutate(refl_array = ((R_tar_a - R_str) / (R_ref_a - R_str) ) * refl_panel,
    refl_array_etienne = ((R_tar_a - R_str) / (R_ref_a - R_str) ) * refl_panel * (1 / (1 - G))   ,
    refl_array_jeremy = array1_jer$refl_array_jeremy)

# gather
array_long <- array1 %>% 
  select(wvl_num, refl_panel, refl_p, refl_array, refl_array_etienne, refl_array_jeremy) %>% 
  rename(wvl = wvl_num,
         panel = refl_panel,
         paper = refl_p,
         array_gap = refl_array,
         array_corr_etienne = refl_array_etienne,
         array_corr_jeremy = refl_array_jeremy) %>% 
  gather(key = target, value = refl, -wvl)

# make plot
refl_plot <- ggplot(array_long, aes(x = wvl, y = refl)) +
  geom_line(aes(colour = target)) +
  ylab('Reflectance') +
  xlab('Wavelength (nm)')
refl_plot
ggsave('refl_plot.pdf', width = 6, height = 4.5)


##### Transmittance ----

# H: Transmittance, leaf array, reference (T_ref_a) for leaf arrays 1-6
T_ref_a <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'H: Transmittance')
T_ref_a_file <- T_ref_a$file_name[1] # first reference

# I: Transmittance, leaf array, target (T_tar_a) for leaf arrays 1-6
T_tar_a <- child_sub %>% 
  filter(sphere_configuration_svc_small_leaves == 'I: Transmittance')
T_tar_a_files <- T_tar_a$file_name


# leaf array 1
T_tar_a_1_path <- paste0(pref, T_tar_a_files[1], '.sig.txt')
T_tar_a_1_raw <- read.table(T_tar_a_1_path, skip = 19) %>% 
  rename(wvl = V1, T_tar_a = V2)


# ref, leaf array 1-6
T_ref_a_path <- paste0(pref, T_ref_a_file, '.sig.txt')
T_ref_a_raw <- read.table(T_ref_a_path, skip = 19) %>% 
  rename(wvl = V1, T_ref_a = V2)


# put together for leaf array 1
trans_array1_df <- T_tar_a_1_raw %>% 
  mutate(T_ref_a = T_ref_a_raw$T_ref_a) %>% 
  mutate(wvl_num = as.numeric(as.character(wvl) ) ) %>% 
  filter(wvl_num <= 2513, wvl_num >= 338) %>% 
  mutate(array = 1,
         T_tar_a = as.numeric(as.character(T_tar_a)),
         T_ref_a = as.numeric(as.character(T_ref_a)))

# calculate gap fraction
trans_gap <- trans_array1_df %>% 
  filter(wvl_num == 400) %>% 
  mutate(gap_frac = T_tar_a / T_ref_a )
trans_G <- trans_gap$gap_frac

# get corrected transmittance from jeremy's files
trans_array1_jer <- leaves %>% 
  select(wavelength, calculated_value, leaf_number, `reflectance-transmittance`) %>%
  rename(wvl_num = wavelength, trans_array_jeremy = calculated_value, reflectance_transmittance = `reflectance-transmittance` ) %>% 
  filter(leaf_number == 1, reflectance_transmittance == 'transmittance')


# calculate transmittance of leaf array 1
trans_array1 <- trans_array1_df %>% 
  mutate(trans_array = T_tar_a / T_ref_a,
         trans_array_etienne = ( (T_tar_a / T_ref_a) - trans_G ) * (1 / (1 - trans_G)),
         trans_array_jeremy = trans_array1_jer$trans_array_jeremy)


# gather
trans_array_long <- trans_array1 %>% 
  select(wvl_num, trans_array, trans_array_etienne, trans_array_jeremy) %>% 
  rename(wvl = wvl_num,
         array_gap = trans_array,
         array_corr_etienne = trans_array_etienne,
         array_corr_jeremy = trans_array_jeremy) %>% 
  gather(key = target, value = trans, -wvl)


# make plot
trans_plot <- ggplot(trans_array_long, aes(x = wvl, y = trans)) +
  geom_line(aes(colour = target)) +
  ylab('Transmittance') +
  xlab('Wavelength (nm)')
trans_plot
ggsave('trans_plot.pdf', width = 6, height = 4.5)
