##
## Function to wrangle metadata file for diurnal LI600 metadata (multiple measurements collected throughout one day)
## 


wrangle_Li600_metadata_diurnal <- function(metadata_diurnal) {
  
  # selecting relevant columns from metadata
  fluColNames = c('Li600_1', "Li600_2", "Li600_3", "Li600_D") # three replicate measurements + 1 dark-adapted measurement
  
  # new wrangled dataframe
  metadata_long_diurn <- metadata_diurnal %>%
   # changing measurement ID column names
    mutate(Li600_D = Dark_Li600) %>%
    mutate(Li600_1 = as.integer(Li600_1), 
           Li600_2 = as.integer(Li600_2), 
           Li600_3 = as.integer(Li600_3)) %>%
    
    # reformatting Hour column
    mutate(Hour = as.character(Hour)) %>%
    mutate(Hour = substring(Hour, 1, 5)) %>%
    
    # pivoting dataframe, reformatting 'leaf' column
    pivot_longer((fluColNames), names_to="leaf", values_to="Obs#") %>%
    mutate(leaf = substring(leaf, 7)) %>%
    
    # creating LightDark variable to differentiate between light-adapted and dark-adapted measurements
    mutate(LightDark = NA) %>%
    mutate_at(vars(LightDark),
              ~ case_when(leaf == "1" | leaf =="2" | leaf=="3" ~"L",
                          leaf == "D" ~"D")) %>%
    
    # reformatting date colummn
    mutate(Date = as.Date(Date, "%m/%d/%y")) 
  
  metadata_long_diurn
}


