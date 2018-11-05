# Script spectra Alizee
# Etienne Laliberte, Aug 23, 2018

### Import JSON data ----

library(googledrive)
library(tidyverse)

### Import CSV data----
spectra_csv <- read.csv('https://web.fulcrumapp.com/shares/5c5290b884ab3734.csv')
spectra_child_csv <- read.csv('https://web.fulcrumapp.com/shares/5c5290b884ab3734.csv?child=measurements')

# get only Aliz spectra
spectra_aliz <- spectra_csv %>%
  filter(project == '2018-Girard-MSc-UdeM')
nrow(spectra_aliz) # 109 plants

# get only Aliz child spectra records
spectra_child_aliz <- spectra_aliz %>%
  inner_join(by = c('' = ''))