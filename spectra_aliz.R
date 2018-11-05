# Script spectra Alizee
# Etienne Laliberte, Aug 23, 2018

### Import JSON data ----

library(googledrive)
library(tidyverse)
library(jsonlite)

# get authorisation to access CABO Google Drive
gs_auth()

# get JSON data
prefix <- 'CABO/Backup_Fulcrum'

# Leaf spectra records
leaf_spectra <- drive_ls(paste0(prefix, '/Leaf_Spectra'))
leaf_spectra_records <- drive_download(file = filter(leaf_spectra,
                                                     name == 'Leaf_Spectra_records.json') )


spectra_json <- fromJSON('Leaf_Spectra_records.json')


### Import CSV data----
spectra_csv <- read.csv('https://web.fulcrumapp.com/shares/5c5290b884ab3734.csv')
