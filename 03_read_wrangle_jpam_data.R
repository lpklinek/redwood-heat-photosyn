# Reading and wrangling junior-PAM data -------------------------------------
# Written by Lily Klinek (lpklinek@ucdavis.edu)



## Preparation and reading data -----

# source necessary functions
source('./R_functions/wrangle_jpam.R')


# specifying list of diurnal-measurement dates
diurnal_dates <- as.Date(c('8/17/23', '8/18/23', '9/14/23', '9/21/23',
                           '10/5/23', '10/6/23', '10/20/23', '10/25/23',
                           '4/7/24', '4/8/24', '5/2/24', '5/3/24', 
                           '5/19/24', '5/20/24', '6/5/24', '6/6/24',
                           '6/26/24', '6/27/24', '8/8/24', '8/9/24'), format = "%m/%d/%y")

# listing all csv files
files <- list.files("./data/raw/03_jpam_data_raw/", full.names = TRUE, pattern = "\\.csv$")

# metadata filepath
meta <- "./data/raw/00_metadata/klinek_metadata_complete.csv"

metadata <- read_csv(meta) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))



## jPAM analysis / initial cleaning v2 -----

# joining metadata to data
all_jpam_files <- tibble(filepath = files) %>%
  mutate(filename = basename(filepath) %>% 
           str_remove("\\.csv$"),  # remove .csv extension
         filename_date = str_extract(filename, "\\d+")) %>%
  left_join(metadata, by = c("filename" = "jpam_directory"))

# creating df of diurnal data
diurnal_jpam <- all_jpam_files %>%
  filter(Date %in% diurnal_dates)

# df of non-diurnal data
non_diurnal_jpam <- all_jpam_files %>%
  filter(!Date %in% diurnal_dates)


allTreeIDs <- c("GarciaE", "GarciaG", "GarciaH")



# for diurnal data
clean_diurnal_data <- diurnal_jpam %>%
  pull(filepath) %>%
  lapply(wrangle_jpam) %>%
  bind_rows() %>%
  select(-c(Time..abs.ms., Time..rel.ms.))

# for non-diurnal data
clean_non_diurnal_data <- non_diurnal_jpam %>%
  pull(filepath) %>%
  lapply(wrangle_jpam) %>%
  bind_rows() %>%
  select(-c(Temp, AHum, Oxyg., X1, X2, Time..abs.ms., Time..rel.ms.))


clean_jpam <- bind_rows(clean_diurnal_data, clean_non_diurnal_data) %>%
  filter(!is.na(ETR_max)) %>%
  filter(ETR_max > 16) %>%
  filter(is.finite(ETR_max)) %>%
  filter(!is.na(Fv.Fm)) %>%
  mutate(Site = substr(treeID, 1, nchar(treeID)-1)) %>%
  distinct() 




## Data cleaning -- 
# Removing "bogey" runs 
# Curves were each inspected to find runs with data quality unsuitable for use. 
# These errors likely stemmed from issues with the fiber optic cable for the instrument, 
# which started to degrade towards the end of the field campaign and ultimately needed
# to be replaced. Errors could have also stemmed from insufficient coverage of the instrument
# aperture by leaf area, or the leaf slipping inside the magnetic leaf clip mid-curve and 
# disrupting data collection.



# list of bogey runs -- manually identified
# 
# Position / Date / TreeID / BranchID / Hour
# 1a 2024-05-02 Garcia H, Branch 1 Hour 15
# 1b 2024-05-02 Garcia G, Branch 3 Hour 13
# 1c 2024-05-02 Garcia G, Branch 1 Hour 17
# 1d 2024-05-02 Garcia G, Branch 3 Hour 17

# 2c 2024-06-06 Garcia H, Branch 3 Hour 12

# 3b 2024-04-08 GarciaH, Branch 1 Hour 14
# 3c 2022-09-13 GarciaH, Branch 1, NA
# 3d 2023-10-06 GarciaE, Branch 2, Hour 13
# 4a 2023-08-18, GarciaH, 2, 15
# 4b 2022-09-23, GarciaH, 1, NA
# 4c 2022-10-06, GarciaH, 3, NA
# 4d 2022-11-04, GarciaE, 3, NA
# 5a 2023-01-21, GarciaE, 2, NA
# 5b 2023-08-18, GarciaH, 3, 12
# 5c 2023-09-21, GarciaE, 3, 16
# 5d 2023-09-21, GarciaH, 1, 17
# 6a 2023-10-06, GarciaE, 1, 13
# 6b 2023-10-06, GarciaE, 2, 13
# 6c 2024-03-11, GarciaH, 3, NA

# 7b 2024-04-08, GarciaE, 2, 11
# 7c 2024-04-08, GarciaH, 3, 12
# 7d 2024-04-08, GarciaH, 1, 14
# 8a 2024-04-08, GarciaG, NA, NA
# 8b 2024-05-02, GarciaG, 3, 13
# 8c 2024-05-02, GarciaE, 2, 14
# 8d 2024-05-02, GarciaH, 1, 15
# 9a 2024-05-02, GarciaG, 1, 17
# 9b 2024-05-02, GarciaG, 3, 17
# 9c 2024-06-06, GarciaH, 3, 12
# 9d 2024-06-27, GarciaG, NA, NA
# 10a 2024-09-27, GarciaH, 1, 12
# 10b 2022-09-08, GarciaE, 1, NA
# 10c 2022-09-08, GarciaE, 2, NA
# 10d 2022-09-08, GarciaE, 3, NA
# 11a 2022-09-08, GarciaH, 1, NA
# 11b 2022-09-08, GarciaH, 2, NA
# 11c 2022-09-08, GarciaH, 3, NA

# Create the data frame of runs to delete
runs_to_delete <- data.frame(
  #                     a             b             c             d
  Date = as.Date(c('2024-05-02', '2024-05-02', '2024-05-02', '2024-05-02', #1
                                               '2024-06-06',               #2
                                 '2024-04-08', '2022-09-13', '2023-10-06', #3
                   '2023-08-18', '2022-09-23', '2022-10-06', '2022-11-04', #4
                   '2023-01-21', '2023-08-18', '2023-09-21', '2023-09-21', #5
                   '2023-10-06', '2023-10-06', '2024-03-11',               #6
                                 '2024-04-08', '2024-04-08', '2024-04-08', #7
                   '2024-04-08', '2024-05-02', '2024-05-02', '2024-05-02', #8
                   '2024-05-02', '2024-05-02', '2024-06-06', '2024-06-27', #9
                   '2024-09-27', '2022-09-08', '2022-09-08', '2022-09-08', #10
                   '2022-09-08', '2022-09-08', '2022-09-08')),             #11
  
  #                     a             b             c             d
  treeID =        c('GarciaH',    'GarciaG',    'GarciaG',    'GarciaG',   #1
                                                'GarciaH',                 #2
                                  'GarciaH',    'GarciaH',    'GarciaE',   #3
                    'GarciaH',    'GarciaH',    'GarciaH',    'GarciaE',   #4
                    'GarciaE',    'GarciaH',    'GarciaE',    'GarciaH',   #5
                    'GarciaE',    'GarciaE',    'GarciaH',                 #6
                                  'GarciaE',    'GarciaH',    'GarciaH',   #7
                    'GarciaG',    'GarciaG',    'GarciaE',    'GarciaH',   #8
                    'GarciaG',    'GarciaG',    'GarciaH',    'GarciaG',   #9
                    'GarciaH',    'GarciaE',    'GarciaE',    'GarciaE',  #10
                    'GarciaH',    'GarciaH',    'GarciaH'),               #11
  
  #                     a             b             c             d
  BranchID =          c(1,            3,            1,            3,       #1
                                                    3,                     #2
                                      1,            1,            2,       #3
                        2,            1,            3,            3,       #4
                        2,            3,            3,            1,       #5
                        1,            2,            3,                     #6     
                                      2,            3,            1,       #7
                        NA,           3,            2,            1,       #8
                        1,            3,            3,            NA,      #9
                        1,            1,            2,            3,       #10
                        1,            2,            3),                    #11
  
  #                     a             b             c             d
  Hour =              c(15,           13,           17,           17,      #1
                                                    12,                    #2
                                      14,           NA,           13,      #3
                        15,           NA,           NA,           NA,      #4
                        NA,           12,           16,           17,      #5
                        13,           13,           NA,                    #6
                                      11,           12,           14,      #7
                        NA,           13,           14,           15,      #8
                        17,           17,           12,           NA,      #9
                        12,           NA,           NA,           NA,      #10
                        NA,           NA,           NA)                    #11
)

# deleting bogey runs using anti-join
final_clean_jpam <- clean_jpam %>%
  rowwise() %>%
  mutate(Date = as.Date(Date)) %>%
  anti_join(runs_to_delete, by = c("Date", "treeID", "BranchID", "Hour")) %>%
  filter(!is.na(BranchID)) %>%
  filter(!(Date == '2024-05-02'))

## Saving processed output ----

write_csv(final_clean_jpam, './data/processed/jpam_all_processed.csv',
          overwrite=TRUE)
