# Script to import, manipulate and visualise the first leaf spectra data from the CABO project


# STEP 1: FULCRUM API -----------------------------------------------------

# install required packages
if (!'jsonlite' %in% installed.packages() ) install.packages('jsonlite')
if (!'curl' %in% installed.packages() ) install.packages('curl')
if (!'httr' %in% installed.packages() ) install.packages('httr')

# load packages
library(jsonlite)
library(curl)
library(httr)

# Fulcrum API base URL
base <- 'https://api.fulcrumapp.com'

# Fulcrum API token (NOTE: replace NULL by your own Fulcrum API token)
token <- NULL

# Fulcrum ID of record for first leaf spectra data
record_id <- '53777634-5b01-4f36-bc08-f4afd89a9e93'

# Define Fulcrum API endpoint and complete url
endpoint <- paste0('/api/v2/records/', record_id, '.json')
url <- paste0(base, endpoint)

# API GET call with Fulcrum additional headers
call1 <- GET(url, add_headers(Accept = 'application/json',
                              'X-ApiToken' = token))

# extract the record data
obj1 <- fromJSON(rawToChar(call1$content))
spectrum1 <- obj1$record$form_values
# NOTE: Fulcrum data names disappeared, and decimal are rounded to one decimal place... can we fix that?


# extract the panel data
panel_id <- obj1$record$form_values$`4f69`

# Define Fulcrum API endpoint and complete url
endpoint2 <- paste0('/api/v2/records/', panel_id, '.json')
url2 <- paste0(base, endpoint2)

# API GET call with Fulcrum additional headers
call2 <- GET(url2, add_headers(Accept = 'application/json',
                              'X-ApiToken' = token))

# extract the record data
obj2 <- fromJSON(rawToChar(call2$content))
panel1 <- obj2$record$form_values

# STEP 2: IMPORT SPECTRA FILES FROM GOOGLE DRIVE -----------------------------------

# install required packages
if (!'googledrive' %in% installed.packages() ) install.packages('googledrive')
if (!'spectrolab' %in% installed.packages() ) install.packages('spectrolab')
if (!'dplyr' %in% installed.packages() ) install.packages('dplyr')
if (!'tibble' %in% installed.packages() ) install.packages('tibble')
if (!'tidyr' %in% installed.packages() ) install.packages('tidyr')


# load packages
library(googledrive)
library(spectrolab)
library(dplyr)
library(tibble)
library(tidyr)

# get working folder in which spectra files are stored, in Google Drive
working_folder <- spectrum1$`5926`

# download all files from working folder to local directory
files <-drive_ls(working_folder)
mypath <- paste0(getwd(), '/spectra')
for (i in 1:nrow(files)) {
  drive_download(files[i,], path = paste0(mypath,'/',files$name[i]) )
}


# function to extract the wavelengths + counts from .sig file
get_sig_data <- function(filename) {
  con <- file(filename, open = 'r')
  sig_all <- readLines(con)
  data_line <- which(sig_all == "data= ")
  close(con)
  con2 <- file(filename, open = 'r')
  readLines(con2, n = data_line)
  spec_data <- readLines(con2)
  close(con2)
  spec_data <- gsub(',', '.', spec_data)
  spec_data2 <- strsplit(spec_data, '  ')
  spec_data_num <- lapply(spec_data2, function(x) as.numeric(x))
  spec_data_mat <- do.call(rbind, spec_data_num)
  colnames(spec_data_mat) <- c('wvl', 'ref', 'tar', 'refl')
  return(as.data.frame(spec_data_mat) )
}

# apply function to each file
filenames <- list.files(mypath)
paths <- paste0(mypath,'/',filenames)
spec_data_all <- lapply(paths, get_sig_data) ; names(spec_data_all) <- filenames

# convert list to long data frame with filename column
for (i in 1:length(spec_data_all)) spec_data_all[[i]]$filename <- names(spec_data_all)[i]
spec_data_all_df <- bind_rows(spec_data_all)
spec_data_all_df$filename <- factor(spec_data_all_df$filename)

# get all Fulcrum metadata for each file
fulcrum_metadata <- data.frame(
  filename = paste0(spectrum1$`4b5c`$form_values$bbe5, '.sig'),
  sphere_config = unlist(spectrum1$`4b5c`$form_values$cfdd$choice_values),
  leaf_num = spectrum1$`4b5c`$form_values$`02bf`,
  spec_num = spectrum1$`4b5c`$form_values$`47ee`,
  refl_port = unlist(spectrum1$`4b5c`$form_values$`3286`$choice_values),
  trans_port = unlist(spectrum1$`4b5c`$form_values$`09f6`$choice_values)
)


# merge data with metadata
data_meta <- spec_data_all_df %>%
  left_join(fulcrum_metadata, by = 'filename' ) 


# STEP 3: REFLECTANCE CALCULATION -----------------------------------------

# subset of reflectance, target
refl_target <- data_meta %>%
  filter(sphere_config %in% c('C: Reflectance, Target'))

# subset of reflectance, stray light
refl_stray <- data_meta %>%
  filter(sphere_config %in% c('B: Reflectance, Stray light')) %>%
  group_by(wvl) %>%
  summarise(stray = mean(tar))


# subset of reflectance, reference
refl_ref_all <- data_meta %>%
  filter(sphere_config %in% ('A: Reflectance, Reference')) %>%
  droplevels()
spec_min <- min(as.numeric(levels(refl_ref_all$spec_num) ) )

# first
refl_ref_first <- refl_ref_all %>%
  filter(spec_num == spec_min) %>%
  select(wvl, tar) %>%
  rename(ref = tar)

# other refs
refl_ref_others <- refl_ref_all %>%
  filter(spec_num != spec_min)

# get calibrated reflectance of panel
calib_refs <- panel1$`5846`$form_values$`9700`
str(calib_refs)
calib_refs2 <- calib_refs[[1]][, 4] %>%
  rename(wvl = c25a,
         calib.refl = b748) %>%
  mutate(sample_name = 'ref_panel',
         wvl = as.numeric(wvl),
         calib.refl = as.numeric(calib.refl))

# convert to wide format
calib_wide <- spread(calib_refs2, wvl, calib.refl)


# convert to spectra object and resample
calib_spectra <- as.spectra(calib_wide)
wvls <- unique(refl_target$wvl)
wvls.sub <- wvls[wvls <=2500]
calib_resamp <- resample(calib_spectra, wvls.sub )

# re-convert to long format
calib_long <- gather(as.data.frame(calib_resamp), sample_name, ref_panel) %>%
  rename(wvl = sample_name,
         calib.refl = ref_panel) %>%
  mutate(wvl = as.numeric(wvl))

# merge other refs with target
refl_target_ref <- refl_target %>%
  bind_rows(refl_ref_others) %>%
  select(-ref, -refl) %>%
  left_join(refl_stray, by = 'wvl') %>%
  left_join(refl_ref_first, by = 'wvl') %>%
  left_join(calib_long, by = 'wvl') %>%
  filter(wvl < 2500) %>%
  mutate(refl_corr = ( (tar - stray ) / (ref - stray) ) * calib.refl) 


# STEP 4: TRANSMITTANCE CALCULATION -----------------------------------------

# subset of transmittance, target
trans_target <- data_meta %>%
  filter(sphere_config %in% c('E: Transmission, Target'))

# subset of reflectance, reference
trans_ref_all <- data_meta %>%
  filter(sphere_config %in% ('D: Transmission, Reference')) %>%
  droplevels()
trans_spec_min <- min(as.numeric(levels(trans_ref_all$spec_num) ) )

# first
trans_ref_first <- trans_ref_all %>%
  filter(spec_num == trans_spec_min) %>%
  select(wvl, tar) %>%
  rename(ref = tar)

# other refs
trans_ref_others <- trans_ref_all %>%
  filter(spec_num != trans_spec_min)


# merge other refs with target
trans_target_ref <- trans_target %>%
  bind_rows(trans_ref_others) %>%
  select(-ref, -refl) %>%
  left_join(trans_ref_first, by = 'wvl') %>%
  mutate(trans_corr = ( (tar) / (ref) )) 

# STEP 5: REFLECTANCE/TRANSMITTANCE PLOTS -------------------------------------------------------

if (!'ggplot2' %in% installed.packages() ) install.packages('ggplot2')
# load packages
library(ggplot2)


# plot all raw DN
plot_all_rawDN <- ggplot(refl_target_ref, aes(x = wvl, y = tar)) +
  geom_line(aes(colour = sphere_config, group = spec_num)) +
  geom_line(aes(y = stray), colour = 'green') +
  geom_line(aes(y = ref), colour = 'blue') +
  xlab('Wavelength (nm)') +
  ylab('Raw DN')
plot_all_rawDN


# remove overlap between detectors
to.remove <- unique(refl_trans$wvl)[c(497:517, 764:767)]

# bind refl and trans data
refl_trans <- refl_target_ref %>%
  bind_rows(trans_target_ref) %>%
  filter(!wvl %in% to.remove) %>%
  mutate(leaf = paste('Leaf', leaf_num))

# apply Merzlyak et al. (2002) correction for transmittance for each leaf
### Would need to determine which wavelenght where there is negligeable absorptance... for now this correction is not applied
# merzlyak <- function(x) {
#   test = filter(x, trans_corr == max(x[x$sphere_config == 'E: Transmission, Target' & wvl <=1400, ]$trans_corr, na.rm = T) )
#   refl_0 <- filter(x, sphere_config == 'C: Reflectance, Target',
#   wvl == test$wvl)$refl_corr
#   trans_0 <- filter(x, sphere_config == 'E: Transmission, Target',
#                       wvl == test$wvl)$trans_corr
#   g <- (1-refl_0) / trans_0
#   x[x$sphere_config == 'E: Transmission, Target', ]$trans_corr <- g * x[x$sphere_config == 'E: Transmission, Target', ]$trans_corr
#   return(x)
# }
# 
# # bind refl and trans data
# refl_trans2 <- refl_trans %>%
#   group_by(leaf_num) %>%
#   do(merzlyak(.))
  
  
# make a plot of reflectance and transmittance
refl_trans_plot <- ggplot(refl_trans, aes(x = wvl, y = refl_corr)) +
  geom_line(aes(colour = sphere_config, group = spec_num)) +
  geom_line(aes(y = 1 - trans_corr, colour = sphere_config, group = spec_num)) +
  facet_wrap(~leaf) +
  xlab('Wavelength (nm)') +
  ylab('Reflectance or (1 - Transmittance)') +
  scale_y_continuous(limits = c(0,1.05)) +
  scale_colour_discrete(name = 'Sphere configuration') +
  theme_bw()
refl_trans_plot
ggsave(refl_trans_plot, file = 'FirstCABOspectrum_AcerSaccharum_AllSpectra.pdf', width = 10, height = 6)
