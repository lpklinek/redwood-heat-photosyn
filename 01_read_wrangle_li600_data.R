
# LI-COR LI600 wrangling and analysis ------------------------------------

## Preparing for data wrangling -----
# source necessary functions
source('./R_functions/read_Li600.R')
source('./R_functions/wrangle_Li600_metadata_diurnal.R')
source('./R_functions/wrangle_Li600_metadata.R')

# set metadata filepath
meta <- "./data/raw/00_metadata/klinek_metadata_complete.csv"

# list of dates with diurnal data collection
diurnal_dates <- c('8/17/23', '8/18/23', '9/14/23', '9/21/23',
                   '10/5/23', '10/6/23', '10/20/23', '10/25/23',
                   '4/7/24', '4/8/24', '5/2/24', '5/3/24', 
                   '5/19/24', '5/20/24', '6/5/24', '6/6/24',
                   '6/26/24', '6/27/24', '8/8/24', '8/9/24', 
                   '9/26/24', '9/27/24')


## Reading metadata ------
# reading in metadata for non-diurnal measurement dates
metadata <- read_csv(meta) %>%
  dplyr::filter(!Date %in% diurnal_dates) #excluding dates of diurnal measurements

# reading in metadata for diurnal measurement dates
metadata_diurn <- read_csv(meta) %>%
  dplyr::filter(Date %in% diurnal_dates) #including dates of diurnal measurements


## Reading raw data ------
# setting folders for data files
FolderPath <- ("./data/raw/01_li600_data_raw/")
Folders <- list.dirs("./data/raw/01_li600_data_raw/")

# reading in data 
# listing each csv in each date folder within LiCOR directory
files_to_read = list.files(
  path = FolderPath,          # directory to search within
  pattern = ".*LI_COR.*csv$", # regex pattern
  recursive = TRUE,           # search subdirectories
  full.names = TRUE           # return the full path
)

# reading raw Li600 diurnal data and processing
Li600_raw_diurn <- files_to_read %>%
  map_dfr(read_Li600) %>%
  dplyr::filter(Date %in% diurnal_dates) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y")) 

# reading raw Li600 non-diurnal data and processing
Li600_raw <- files_to_read %>%
  map_dfr(read_Li600) %>%
  dplyr::filter(!Date %in% diurnal_dates) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))


## Joining data with metadata -----
# wrangling metadata
metadata_long_diurn <- wrangle_Li600_metadata_diurnal(metadata_diurn) 
metadata_long <- wrangle_Li600_metadata(metadata) 

# join Li600 data to metadata based on Site, Date, Obs, TreeID, and Light/Dark
Li600_meta_diurn <- right_join(Li600_raw_diurn, metadata_long_diurn, by=c('Date', 'Site', 'TreeID', 'Hour', 'Obs#', 'LightDark'))
Li600_meta <- right_join(Li600_raw, metadata_long, by=c('Date', 'Site', 'TreeID', 'Hour', 'Obs#', 'LightDark')) 

## Final dataframe maintenance -----
# all seasonal + diurnal, not averaged
Li600_all_entire <- rbind(Li600_meta, Li600_meta_diurn) %>%
  group_by(Site, Date, LightDark) %>%
  mutate(Tdepr = Tleaf - Tref)

# averaged for each date
Li600_all <- rbind(Li600_meta, Li600_meta_diurn) %>%
  group_by(Site, Date, LightDark) %>% # could add TreeID here if we wanted to show each tree
  summarise_at(vars("gsw":"m_rhr"), ~mean(.x, na.rm = T)) %>%
  mutate(Tdepr = Tleaf - Tref)

# renaming the Fm prime column 
Li600_all['Fm_prime'] = Li600_all[21]
Li600_all_entire['Fm_prime'] = Li600_all_entire[30]


## Saving processed output ----

# all observations
write_csv(Li600_all_entire, './data/processed/li600_all_processed.csv',
          overwrite=TRUE)

# daily averages
write_csv(Li600_all, './data/processed/li600_daily_avgs_processed.csv',
          overwrite=TRUE)



