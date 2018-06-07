# convert all commas as decimal separators in .sig files to .

# install required packages
if (!'googledrive' %in% installed.packages() ) install.packages('googledrive')
if (!'stringr' %in% installed.packages() ) install.packages('stringr')
if (!'spectrolab' %in% installed.packages() ) install.packages('spectrolab')
if (!'tidyverse' %in% installed.packages() ) install.packages('tidyverse')


# load packages
library(googledrive)
library(stringr)
library(spectrolab)
library(tidyverse)


# function to replace commas by dots on individual metadata lines
comma_to_dot <- function(x) {
  tmp <- unlist(strsplit(x, ", ") )
  tmp2 <- str_replace(tmp, ',', '.')
  paste(tmp2, collapse = ', ')
}

# get list of .sig files in the test working directory (Google Drive)
files <- drive_ls(path = '2018-BeauchampRioux-MSc-UdeM/spectra/2018-05-25-JBM_JardinFleuri-2092')

# create directory to store raw spectra
raw_folder <- paste0(getwd(), '/spectra_raw')
dir.create(raw_folder)
for (i in 1:nrow(files)) drive_download(files[i, ], path = paste0(getwd(), '/spectra_raw', '/', files$name[i]), overwrite = T)

temp_folder <- paste0(getwd(), '/spectra_temp')
dir.create(temp_folder)

# convert commas for all .sig files (were saved using the wrong decimal separator...)
for (i in 1:nrow(files)) {
file1 <- file(paste0(getwd(), '/spectra_raw', '/', files$name[i]), open = 'r')
file2 <- file(paste0(temp_folder, '/', files$name[i]), open = 'w')
part1a <- readLines(file1, n = 3)
part1b <-  comma_to_dot(readLines(file1, n = 1) ) # integration
part1c <- readLines(file1, n = 9)
part1d <-  comma_to_dot(readLines(file1, n = 1) ) # temp
part1e <-  comma_to_dot(readLines(file1, n = 1) ) # battery
part1f <- readLines(file1, n = 8)
part1g <-  comma_to_dot(readLines(file1, n = 1) ) # factors
readLines(file1, n = 2) # remove the two inclinometer lines
part1h <- readLines(file1, n = 1)
part1 <- c(part1a, part1b, part1c, part1d, part1e, part1f, part1g, part1h)
part2 <- readLines(file1)
part2 <- gsub(',', '.', part2)

close(file1)
#file1 <- file(files$name[1], open = 'r+')
writeLines(part1, file2)
writeLines(part2, file2)
#writeLines('', file1)
close(file2)
}

# read .sig files
spectra <- read_spectra(temp_folder, format = 'sig')
plot(spectra)


str(spectra)
data.frame(spectra)

### need to 
