# Reading and wrangling Li6800 temperature-response curve data -------------------------------------
# Written by Lily Klinek (lpklinek@ucdavis.edu)


# VPD parameter prep -----
# calculating VPD for temperature curve Li6800
g1_G <- mgcv::gam(VPD_kPa ~ s(Tair, bs="cs"), data = all_meteo[all_meteo$Site == 'Garcia', ])
temp_curve <- tibble(Tair = seq(22, 43, by = 3))
temp_curve$VPD_G <- predict(g1_G, temp_curve)



# Reading and wrangling Li6800 data -----

## Note: the data Excel files produced by the Li6800 do not "calculate" values -- in other words, 
## the values of certain cells will technically still be the formula used to calculate them 
## as opposed to the actual value produced by the calculation. This causes problems when 
## reading data into R, because the formula cells will be read in as 0s. 
## To fix this, you need to open the excel file, go to the Formulas menu, and click 
## "Calculate Now". This will rewrite the formulas with the actual calculated value. 
## Then, save the file and close it. This has already been done for all data files 
## associated with Klinek et al. 2025, but if utilizing this workflow for other data, this 
## step will need to be done before the processing code can successfully be run. 



# define directory containing Li6800 files
data_dir <- "./data/raw/04_li6800_data_raw/"

# list all Excel files in directory
file_list <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE) # using regex to find xlsx files
file_list <- file_list[!grepl("~\\$", basename(file_list))] # remove temp files

# columns to read (can be edited based on variables of interest)
columns_of_interest <- c("E", "A", "Ca", "Ci", "gsw", "RHcham", "VPDleaf", 
                         "VPDcham", "Qin", "ETR", "Fm", "Fm'", "Fo.", "Fo", "Fo'", "qL",
                         "Fs", "Fv/Fm", "Fv'/Fm'", "NPQ", "PhiPS2", "Tair", "Tleaf")

# creating empty list to hold data
all_data <- list()

for (file_path in file_list) {
  # extract curve metadata from file name
  file_name <- basename(file_path)
  file_info <- strsplit(file_name, "_|\\.|-|/")[[1]]
  date <- paste(c(file_info[1], file_info[2], file_info[3]), collapse='-')
  site <- file_info[6]
  tree_id <- file_info[7]
  
  # read column names
  col_names <- suppressMessages(readxl::read_excel(file_path, range = "A15:KL15", col_names = FALSE)) 
  %>% as.character()
  
  # read only data
  data <- suppressMessages(readxl::read_excel(file_path, col_names = FALSE, skip = 16)) # data starts from row 17
  
  # assign colnames to the data
  colnames(data) <- col_names
  
  # select only columns of interest and add metadata
  data_selected <- suppressMessages({
    data %>%
      select(any_of(columns_of_interest)) %>%
      mutate(Date = date, Site = site, TreeID = tree_id)
  })
  
  # append to the list
  all_data[[file_name]] <- data_selected
}


# combine all data into a single data frame
combined_data <- bind_rows(all_data) %>%
  mutate(
    Site = str_replace_all(Site, "garcia", "Garcia"), # fixing site name capitalization
    TreeID = toupper(TreeID) # fixing tree name capitalization
  )

# Saving processed output ----

write_csv(combined_data, './data/processed/li6800_all_processed.csv',
          overwrite=TRUE)