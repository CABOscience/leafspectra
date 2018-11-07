# calibration of spectralon panels

library(googledrive)
library(tidyverse)
library(spectrolab)
library(signal)

drive_download('SRS-99-010-99AA02-0318-9710.calib', overwrite = T)

panel_9710 <- read.table("SRS-99-010-99AA02-0318-9710.calib") %>% 
  rename(wvl = V1, refl = V2) %>% 
  filter(refl < 1.02)
str(panel_9710)

# plot the data
plot(panel_9710, type = 'l')

# convert to long
panel_9710_long <- panel_9710 %>% 
  spread(key = wvl, value = refl) %>% 
  mutate(SN = 'SRS-99-010-99AA02-0318-9710')
str(panel_9710_long)
panel_9710_spectra <- as.spectra(panel_9710_long, name_idx = ncol(panel_9710_long))                    
plot(panel_9710_spectra)

# plot sensor matching regions
vnir_plot <- ggplot(panel_9710, aes(x = wvl, y = refl) ) +
  geom_line() +
  coord_cartesian(xlim = c(850, 875))
vnir_plot

# matching sensors
panel_9710_match <- match_sensors(panel_9710_spectra, splice_at = 864)
plot(panel_9710_match)
panel_9710_matrix <- as.matrix(panel_9710_match)

# try smoothing fonction in spectrolab
panel_9710_smooth <- smooth(panel_9710_match)
plot(panel_9710_smooth)

# apply Savitzky-Golay filter
panel_9710_sg55 <- sgolayfilt(panel_9710_matrix, p = 3, n = 55)
panel_9710_smooth_df <- data.frame(wvl = panel_9710$wvl,
                               sgolay55 = panel_9710_sg55,
                               raw = panel_9710$refl,
                               match = panel_9710_matrix[1,]) 
sgolay_plot <- ggplot(panel_9710_smooth_df, aes(x = wvl, y = raw)) +
  geom_line(alpha = .5) +
  geom_line(aes(y = match), alpha = .5, col = 'blue') + 
  geom_line(aes(y = sgolay55), col = 'red')
sgolay_plot +
  ylab('Reflectance') +
  xlab('Wavelength (nm)')
ggsave('9710_sgolay.png', width = 6, height = 4.5, dpi = 600)

# save a new calib file to google drive
write.table(file = 'SRS-99-010-99AA02-0318-9710_smooth.calib', matrix(c(panel_9710_smooth_df$wvl, panel_9710_sg55), byrow = F, ncol = 2, nrow = length(panel_9710_sg55)),
            quote = F,
            row.names = F,
            col.names = F,
            sep = '\t') 
drive_upload('SRS-99-010-99AA02-0318-9710_smooth.calib', path = '/CABO/DATA/SPECTROSCOPY/PANELS/99AA02-0318-9710/2018-04-10/')
